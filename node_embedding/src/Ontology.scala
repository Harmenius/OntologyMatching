import cc.factorie.app.nlp.embeddings.FastLineReader
import com.hp.hpl.jena.graph.Node_Literal
import com.hp.hpl.jena.ontology.OntResource
import com.hp.hpl.jena.ontology.impl.OntModelImpl
import com.hp.hpl.jena.rdf.model.{Model, RDFNode, Statement}
import com.hp.hpl.jena.rdf.model.impl.LiteralImpl
import com.hp.hpl.jena.util.FileManager
import com.hp.hpl.jena.vocabulary.RDFS

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

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
  val LABEL_PROPERTY = RDFS.label
  def get_label(uri: String) : String = {
    val n = model.getResource(uri)
    val e = model.listStatements(n, model.getProperty(LABEL_URI), null).nextStatement()
    val l = e.getObject.asLiteral().getLexicalForm
    clean(l)
  }

  val model : Model = FileManager.get.loadModel(filename)
  printf("%s contains %d nodes and %d edges%n", filename, getNodes.size, getEdges.size)

  def getNodes: Iterator[String] = {
    val nodes = getEdges.map(_.split(" ")).flatMap[String](a => Array(a(0),a(2)))
    if(this.reference)
      return nodes
    else
      return nodes.toSet.toIterator.filterNot(_.contains("blank:")).toSet.toIterator
  }


  def filterlabelless(strings: Iterator[String]): Iterator[String] = {
    val stringlist = strings.toList
    val prefix = "http://" + filename.split("/").last + "#"
    val fullstrings = stringlist.map(str => str.split(" ").map(prefix + _))

    val nodes = fullstrings.map(uris => (model.getResource(uris(0)), model.getResource(uris(2))))
    stringlist.iterator
  }

  def filterblank(strings: Iterator[String]): Iterator[String] = {
    strings.toSet.toIterator.filterNot(s => s.contains("blank:") || s.contains("Thing")).toSet.toIterator
  }

  def getEdges: Iterator[String] = {
    val stmtit = reference match {
      case false =>
        //model.listStatements(null, model.getProperty(EDGE_URIS(0)), null) andThen
        model.listStatements(null, model.getProperty(EDGE_URIS(1)), null) //andThen
        //model.listStatements(null, model.getProperty(EDGE_URIS(2)), null)
      case true =>
        model.listStatements(null, model.getProperty(REF_URIS(0)), null) andThen
        model.listStatements(null, model.getProperty(REF_URIS(1)), null) andThen
        model.listStatements(null, model.getProperty(REF_URIS(2)), null)
    }
    val edges = stmtit.asScala.map(this.toString)
    if(this.reference)
      return edges
    else
      return filterblank(filterlabelless(edges))
  }
}

class SharedOntology() extends Ontology {
  var ontologies: Array[Ontology] = null
  var ontology_index: Map[String, Int] = null

  def this(ontologies: Seq[Ontology]) {
    this()
    this.ontologies = ontologies.toArray
  }

  def this(filenames: String) {
    this()
    printf("Building ontology from %s.%n", filenames)
    ontologies = new Array[Ontology](filenames.split(";").length)
    ontology_index = filenames.split(";").zipWithIndex.map{case (fn, i) => fn -> i}.toMap
    for ((filename, i) <- filenames.split(";").zipWithIndex) {
      if (filename.contains(".csv") || filename.contains(".tar.gz")) {
        ontologies.update(i, new CSVOntology(filename))
      }
      else if (filename.endsWith(".rdf") || filename.endsWith(".owl") || filename.endsWith(".ttl"))
        ontologies.update(i, new RDFOntology(filename))
      else {
        throw new Exception()
      }
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
      it ++= ontologies(i).getEdges
    }
    it
  }

  def getOntologies = ontologies
  def getOntology(fn: String) = {
    if (ontology_index != null && ontology_index.contains(fn))
      ontologies(ontology_index(fn))
    else {
      println("Could not find ontology in shared ontology. Here's an empty one.")
      new EmptyOntology
    }
  }
}

class CSVOntology(filename: String) extends Ontology {

  override def getNodes: Iterator[String] = {
    val nodes = new FastLineReader(filename).map[Array[String]](node => node.split(" "))
    nodes.filter(e => e.size == 3).flatMap[String](a => Array(a(0), a(2)))
  }

  override def getEdges: Iterator[String] = {
    new FastLineReader(filename).map(clean(_))
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

class mutableOntology() extends Ontology {
  val nodes = new mutable.HashSet[String]
  val edges = new mutable.HashSet[(String, String)]

  def getNodeset = nodes
  def getEdgeset = nodes

  override def getNodes: Iterator[String] = nodes.iterator

  override def getEdges: Iterator[String] = edges.map(t => t._1 + " " + t._2).iterator
  
  def addNode(n: String): Unit = {
    nodes.add(n)
  }

  def addEdge(a: String, b: String): Unit = {
    nodes.add(a)
    nodes.add(b)
    edges.add((a,b))
  }
}