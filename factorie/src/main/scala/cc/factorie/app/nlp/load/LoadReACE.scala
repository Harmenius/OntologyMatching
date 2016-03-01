/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.load
import java.io.File

import cc.factorie.app.nlp.{Document, Sentence, Token, UnknownDocumentAnnotator, _}
import cc.factorie.app.nlp.coref.Mention
import cc.factorie.app.nlp.ner.ConllNerSpan
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.variable.SetVariable

import scala.Array.fallbackCanBuildFrom
import scala.xml.{NodeSeq, XML}

trait ReACEMentionIdentifiers {
  val mId: Option[String]
  val eId: Option[String]
  val headStart: Option[Int]
  val headEnd: Option[Int]
  val headLength: Option[Int]
  val mType: String
  val mSubType: String
}

trait ReACERelationIdentifiers {
  val rId: Option[String]
  val rType: Option[String]
  val rSubtype: Option[String]
}

trait ReACESentenceAnnotations {
  val paragraphId: Option[String]
  val sentenceId: Option[String]
}

trait ReACEWordAnnotations {
  val lemma: Option[String]
  val pos: Option[String]
  val chunk: Option[String]
  val nounHead: Option[String]
  val verbStem: Option[String]
  val verbHead: Option[String]
  val verbVoice: Option[String]
  val verbNeg: Option[String]
}

class ReACESentenceId(val sentId: String)

object LoadReACE {

  private def getAttr(ns: NodeSeq, key: String): Option[String] = {
    val fullKey: String = "@" + key
    val v = (ns \ fullKey).text
    if (v == "") None
    else Some(v)
  }

  private def makeTokenAnnotations(wordXml: NodeSeq): ReACEWordAnnotations = {
    val a: String => Option[String] = getAttr(wordXml, _)
    new ReACEWordAnnotations {
      val lemma: Option[String] = a("l")
      val pos: Option[String] = a("p")
      val chunk: Option[String] = a("phr")
      val nounHead: Option[String] = a("headn")
      val verbStem: Option[String] = a("vstem")
      val verbHead: Option[String] = a("headv")
      val verbVoice: Option[String] = a("voice")
      val verbNeg: Option[String] = a("neg")
    }
  }

  private def makeDoc(xml: String): Document = {
    val doc = new Document().setName(xml)
    doc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass
    doc.annotators(classOf[PennPosTag]) = UnknownDocumentAnnotator.getClass

    doc.attr += new ACEFileIdentifier(xml)
    val xmlText: NodeSeq = XML.loadFile(xml + ".ttt.xml")

    var currP = 0
    for (p <- xmlText \\ "p") {
      currP += 1
      for (s <- p \\ "s") {
        val sId = getAttr(s, "id")
        val sent = new Sentence(doc)
        sent.attr += new ReACESentenceAnnotations {
          val paragraphId = Some(currP.toString)
          val sentenceId = sId
        }
        for (w <- s \\ "w") {
          val t = new Token(sent, w.text)
          doc.appendString(" ")
          val annotations = makeTokenAnnotations(w)
          t.attr += annotations // TODO I think these annotations should go in more standard FACTORIE NLP form -akm
          annotations.pos.foreach(p => t.attr += new PennPosTag(t, p))
        }
      }
    }
    doc
  }

  private def lookupEntityMention(doc: Document, id: String): Option[Mention] = {
    val opt = doc.attr[ner.ConllNerSpanBuffer].find {
      s => {
        val a = s.attr[ReACEMentionIdentifiers]
        (a ne null) && a.mId.get == id
      }
    }
    if (opt == None) None
    else Some(opt.get.asInstanceOf[Mention])
  }

  def addNrm(doc: Document, xml: String): Document = {
    val coref = doc.getCoref
    var xmlText: NodeSeq = XML.loadFile(xml + ".nrm.xml")
    assert(doc.attr[ACEFileIdentifier].fileId == xml) // adding to the right document?

    // Add mentions
    for (mention <- xmlText \\ "ne") {
      // named-entity mentions
      // phrase span
      val start = (mention \ "@fr").text.drop(1).toInt - 1
      val end = (mention \ "@to").text.drop(1).toInt - 1
      val length = end - start + 1

      // head span
      val hstart = (mention \ "@hfr").text.drop(1).toInt - 1
      val hend = (mention \ "@hto").text.drop(1).toInt - 1
      val hlength = hend - hstart + 1

      // ner type
      val nerType = (mention \ "@t").text
      val nerSubType = (mention \ "@st").text

      val phrase = new Phrase(doc.asSection,start,length,hend)
      phrase.attr += new ConllNerSpan(doc.asSection,start,length,nerType)

      val m = coref.addMention(phrase)

      m.attr += new ReACEMentionIdentifiers {
        val mId = getAttr(mention, "id")
        val eId = getAttr(mention, "gid")
        val headStart = Some(hstart)
        val headEnd = Some(hend)
        val headLength = Some(hlength)
        val mType = nerType
        val mSubType = nerSubType
      }
    }

    // Add relations
    xmlText = XML.loadFile(xml + ".nrm.xml") // is there a way to avoid rereading?
//    doc.attr += new RelationMentions
//    for (rel <- xmlText \\ "rel") {
//      val ids = new ReACERelationIdentifiers {
//        val rId = getAttr(rel, "id")
//        val rType = getAttr(rel, "t")
//        val rSubtype = getAttr(rel, "st")
//      }
//
//      val e1 = lookupEntityMention(doc, getAttr(rel, "e1").get).get
//      val e2 = lookupEntityMention(doc, getAttr(rel, "e2").get).get
//      val args = Seq(e1, e2)
//
//      val m = new RelationMention(e1, e2, ids.rType.get, Some(ids.rSubtype.get))
//      m.attr += ids
//      doc.attr[RelationMentions].add(m)(null)
//      args.foreach(_.attr.getOrElseUpdate(new RelationMentions).add(m)(null))
//    }

    doc
  }

  class MentionsSet extends SetVariable[Mention]
  // TODO: consider renaming this to fromFile to match the API for other loaders.
  // But if renamed, how can the user know that ttt.xml is required?
  def fromTtt(ttt: String): Document = {
    val fileStr = ttt.dropRight(8)
    val doc = makeDoc(fileStr)
    addNrm(doc, fileStr)
    doc
  }

  def fromDirectory(dir: String, takeOnly: Int = Int.MaxValue): Seq[Document] =
    new File(dir).listFiles().filter(_.getName.endsWith(".ttt.xml")).take(takeOnly).map(f => fromTtt(f.getAbsolutePath))

  def main(args: Array[String]): Unit = {
    val docs = fromDirectory(args(0))
    for (d <- docs)
      d.attr[ner.ConllNerSpanBuffer].foreach(s => println(s))
  }

}
