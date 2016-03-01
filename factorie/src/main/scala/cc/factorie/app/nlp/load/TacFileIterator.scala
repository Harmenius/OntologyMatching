package cc.factorie.app.nlp.load

import java.io._
import java.util.Scanner
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp.Document

object TACDocTypes {
  sealed trait TACDocumentType
  case object Newswire extends TACDocumentType
  case object DiscussionForum extends TACDocumentType
  case object WebDocument extends TACDocumentType

  object TACDocumentType {
    def fromFilePath(f:File):TACDocumentType = {
      val path = f.getAbsolutePath.toLowerCase
      if(path.contains("discussion_forums")) {
        DiscussionForum
      } else if(path.contains("newswire")) {
        Newswire
      } else if(path.contains("web")) {
        WebDocument
      } else {
        throw new Exception("Unable to assign document at path %s to a document type".format(path))
      }
    }
  }
}


/**
 * @author John Sullivan
 */
class TacFileIterator(tacDocFile:File) extends Iterator[Document] {
  import TACDocTypes._

  private val docEndString = """</doc>"""
  private val webDocStartString = """<DOC>"""
  private val docIdRegex = """(?i)<DOC ID="([^"]+)"[^>]*>""".r
  private val webDocIdRegex = """(?i)<DOCID> ([^ ]+) </DOCID>""".r

  /** we use scanner here so that when we recreate the lines by adding \n we don't change
    * the character count on documents that may use crlf to delimit lines
    */
  private val tacReader = new Scanner(if(tacDocFile.getName.endsWith(".gz")) {
    new GZIPInputStream(new FileInputStream(tacDocFile))
  } else {
    new FileInputStream(tacDocFile)
  }).useDelimiter("\n")

  private var docBuffer = new StringBuilder()
  private var line = null.asInstanceOf[String]
  private var lineNum = 0

  // grouping together to avoid forgetting something
  @inline
  private def advanceLine() {
    docBuffer append line
    docBuffer append "\n"
    line = if(tacReader.hasNext) tacReader.next() else null
    lineNum += 1
  }

  //priming the pump - we don't call advanceLine because we don't want to add a null to the start of our doc
  line = if(tacReader.hasNext) tacReader.next() else null
  lineNum += 1

  def next() = {

    val docIdMatchOpt = docIdRegex.unapplySeq(line).map(_.head)

    // We should be at the start of a new document here, otherwise we have a problem.
    assert(line.equalsIgnoreCase(webDocStartString) || docIdMatchOpt.isDefined, "Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, tacDocFile.getName))
    val docId = if(docIdMatchOpt.isDefined) {
      docIdRegex.unapplySeq(line).get.head
      //var docIdRegex(docId) = line
    } else if(line equalsIgnoreCase webDocStartString) { // we know that one must be true but let's not tempt fate
      advanceLine()
      //var webDocIdRegex(docId) = line
      webDocIdRegex.unapplySeq(line).get.head
    } else {
      throw new Exception("Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, tacDocFile.getName))
    }

    while(!line.equalsIgnoreCase(docEndString)) {
      advanceLine()
    }
    // the loop exits when the doc end is found, but that us still part of the previous document so we need to consume it.
    advanceLine()
    val docString = docBuffer.toString()
    docBuffer = new StringBuilder()
    val doc = new Document(docString).setName(docId)
    doc.attr += TACDocumentType.fromFilePath(tacDocFile)
    doc.annotators += classOf[TACDocumentType] -> this.getClass
    doc
  }

  def hasNext = line != null
}

object TacFileIterator {
  def main(args:Array[String]) {
    val f = new File(args(0))

    val doc = new TacFileIterator(f).next()
    println(doc.name)
    val wrt = new BufferedWriter(new FileWriter(doc.name))
    wrt.write(doc.string)
    wrt.flush()
    wrt.close()

  }
}
