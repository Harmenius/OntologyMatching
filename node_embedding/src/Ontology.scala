import cc.factorie.app.nlp.embeddings.FastLineReader
import com.hp.hpl.jena.graph.Node_Literal
import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.rdf.model.impl.LiteralImpl
import com.hp.hpl.jena.util.FileManager

import scala.collection.JavaConverters._

abstract class Ontology {
  // TODO move to object

  def clean(name: String): String = {
    val nameparts = name.split(":")
    nameparts(nameparts.length-1)
  }

  def toString(node: RDFNode): String = {
    if(node.isLiteral)
      clean(node.asNode.getLiteral.getLexicalForm)
    else if(node.isResource) {
      val name = node.asResource.getLocalName
      if (name == null)
        return "blank:%s".format(node.toString)
      name
    }
    else
      "b"
  }

  def toString(edge: Statement): String =
    "%s %s %s".format(toString(edge.getObject).replace(" ", "_"),
      edge.getPredicate.toString.replace(" ", "_"),
      toString(edge.getSubject).replace(" ", "_"))

  def toString(node: String): String = node

  def getNodes: Iterator[String]
  def getEdges: Iterator[String]
}

class RDFOntology (filename: String, ignoreblank: Boolean = true) extends Ontology {
  val model : Model = FileManager.get.loadModel(filename)

  def getNodes: Iterator[String] = {
    var it = model.listObjects.asScala ++
      model.listSubjects.asScala
    if(WordSenseOpts.includeEdgeLabels.value) {
      it = it ++
        model.listStatements.asScala.map(s => s.getPredicate.asResource)
    }
    it.toSet[RDFNode].filterNot(node => node.isAnon).toIterator.map(this.toString)
  }

  def getEdges: Iterator[String] = {
    def containsBlank(n : Statement) : Boolean = {
      (n.getObject.isResource && n.getObject.asResource.isAnon) ||
        (n.getSubject.isResource && n.getSubject.asResource.isAnon)
    }
    var edges = model.listStatements().asScala
    if (ignoreblank)
      edges = edges.filterNot(containsBlank)
    edges.map(this.toString)
  }

}

class SharedOntology(filenames: String) extends Ontology {
  val ontologies = new Array[Ontology](filenames.split(";").length)

  init(filenames)
  def init(filenames: String): Unit = {
    printf("Building ontology from %s.%n", filenames)
    for ((filename, i) <- filenames.split(";").zipWithIndex) {
      if (filename.contains(".csv") || filename.contains(".tar.gz")) {
        ontologies.update(i, new CSVOntology(filename))
      }
      if (filename.endsWith(".rdf") || filename.endsWith(".owl"))
        ontologies.update(i, new RDFOntology(filename))
    }
  }

  override def getNodes: Iterator[String] = {
    var it = ontologies(0).getNodes
    for (i <- 1 until ontologies.length) {
      it = it ++ ontologies(i).getNodes
    }
    it
  }

  override def getEdges: Iterator[String] = {
    var it = ontologies(0).getEdges
    for (i <- 1 until ontologies.length) {
      it = it ++ ontologies(i).getEdges
    }
    it
  }

  def getOntologies = ontologies
}

class CSVOntology(filename: String) extends Ontology {
  val reader = new FastLineReader(filename)

  override def getNodes: Iterator[String] = {
    reader.flatMap[String](node => node.split(" "))
  }

  override def getEdges: Iterator[String] = {
    reader
  }
}