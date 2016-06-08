import cc.factorie.app.nlp.embeddings.FastLineReader
import com.hp.hpl.jena.graph.Node_Literal
import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.rdf.model.impl.LiteralImpl
import com.hp.hpl.jena.util.FileManager

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

abstract class Ontology {
  val LABEL_URI = "http://www.w3.org/2000/01/rdf-schema#label"
  val EDGE_URIS = Array("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym",
                        "http://www.w3.org/2000/01/rdf-schema#subClassOf",
                        "http://www.geneontology.org/formats/oboInOwl#hasDefinition")
  val REF_URIS = Array("http://knowledgeweb.semanticweb.org/heterogeneity/alignmententity1",
                       "http://knowledgeweb.semanticweb.org/heterogeneity/alignmententity2",
                       "http://knowledgeweb.semanticweb.org/heterogeneity/alignmentmeasure")

  def clean(name: String): String = {
    val nameparts = name.split(":")
    nameparts(nameparts.length-1).replace(" ", "_").toLowerCase
  }

  def toString(node: RDFNode): String = {
    if(node.isLiteral) {
      //val lblNode = node.asResource().getProperty(node.getModel.getProperty(LABEL_URI)).getObject.asLiteral.getLexicalForm
      clean(node.asLiteral().getLexicalForm)
    } else if(node.isResource) {
      val name = node.asResource.getLocalName
      if (name == null)
        return "blank:%s".format(node.toString)

      val lblEdge = node.asResource().getProperty(node.getModel.getProperty(LABEL_URI))
      if (lblEdge == null)
        return name
      val lblNode = lblEdge.getObject.asLiteral.getLexicalForm
      clean(lblNode)
    } else "b"
  }

  def toString(edge: Statement): String =
    "%s %s %s".format(
      toString(edge.getObject),
      edge.getPredicate.toString.replace(" ", "_"),
      toString(edge.getSubject)
    )

  def toString(node: String): String = node

  def getNodes: Iterator[String]
  def getEdges: Iterator[String]
}

class RDFOntology (filename: String, reference: Boolean = false) extends Ontology {
  def get_label(uri: String) : String = {
    val n = model.getResource(uri)
    val e = model.listStatements(n, model.getProperty(LABEL_URI), null).nextStatement()
    val l = e.getObject.asLiteral().getLexicalForm
    clean(l)
  }

  val model : Model = FileManager.get.loadModel(filename)

  def getNodes: Iterator[String] = {
    val stmtit = model.listStatements(null, model.getProperty(LABEL_URI), null).asScala
    stmtit.map(s => s.getObject.asLiteral.getLexicalForm)
  }

  def getEdges: Iterator[String] = {
    val stmtit = reference match {
      case false =>
        model.listStatements(null, model.getProperty(EDGE_URIS(0)), null) andThen
        model.listStatements(null, model.getProperty(EDGE_URIS(1)), null) andThen
        model.listStatements(null, model.getProperty(EDGE_URIS(2)), null)
      case true =>
        model.listStatements(null, model.getProperty(REF_URIS(0)), null) andThen
        model.listStatements(null, model.getProperty(REF_URIS(1)), null) andThen
        model.listStatements(null, model.getProperty(REF_URIS(2)), null)
    }
    stmtit.asScala.map(this.toString)
  }
}

class SharedOntology(filenames: String) extends Ontology {
  val ontologies = new Array[Ontology](filenames.split(";").length)
  val ontology_index = filenames.split(";").zipWithIndex.map{case (fn, i) => fn -> i}.toMap

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
  def getOntology(fn: String) = {
    if (ontology_index.contains(fn))
      ontologies(ontology_index(fn))
    else {
      println("Could not find ontology in shared ontology. Here's an empty one.")
      new EmptyOntology
    }
  }
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

class EmptyOntology() extends Ontology {
  override def getNodes: Iterator[String] = {
    Array[String]().iterator
  }

  override def getEdges: Iterator[String] = {
    Array[String]().iterator
  }
}

