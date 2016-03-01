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
import cc.factorie.app.nlp.{Document, Sentence, Token, UnknownDocumentAnnotator}
import cc.factorie.app.nlp.ner._
import cc.factorie.util.FastLogging

import scala.collection.mutable.ArrayBuffer

// Usage:
// Either LoadConll2002.fromFilename("foo")
// or LoadConll2003(BILOU = true).fromFilename("foo")

object LoadConll2002 extends LoadConll2002(false)

case class LoadConll2002(BILOU:Boolean = false) extends Load with FastLogging {
  val conllToPennMap = Map("\"" -> "''", "(" -> "-LRB-", ")" -> "-RRB-", "NN|SYM" -> "NN")

  def fromSource(source:io.Source): Seq[Document] = {
    import scala.collection.mutable.ArrayBuffer
    def newDocument(name:String): Document = {
      val document = new Document("").setName(name)
      document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
      document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
//      document.annotators(classOf[pos.PennPosTag]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
      document
    }

    val documents = new ArrayBuffer[Document]
    var document = newDocument("CoNLL2002-"+documents.length)
    documents += document
    var sentence = new Sentence(document)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        if(sentence.nonEmpty) sentence = new Sentence(document)
      }
      else {
        val fields = line.split(' ')
        assert(fields.length == 2)
        val word = fields(0)
//        val partOfSpeech = conllToPennMap.getOrElse(fields(1), fields(1))
        val ner = fields(1).stripLineEnd
        if (sentence.length > 0) document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new LabeledBioConllNerTag(token, ner)
//        token.attr += new cc.factorie.app.nlp.pos.PennPosTag(token, partOfSpeech)
      }
    }
    if (BILOU) convertToBILOU(documents)
    logger.info("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.tokens.size).sum+" tokens total")
    documents
  }
  def convertToBILOU(documents : ArrayBuffer[Document]) {
    for(doc <- documents) {
      for(sentence <- doc.sentences) {
        for(token <- sentence.tokens) {
          val ner = token.nerTag
          var prev : Token = null
          var next : Token = null
          if(token.sentenceHasPrev) prev = token.sentencePrev
          if(token.sentenceHasNext) next = token.sentenceNext
          token.sentenceNext
          val newLabel : String = IOBtoBILOU(prev, token, next)
          token.attr += new LabeledBilouConllNerTag(token, newLabel)
        }
      }
    }
  }

  def IOBtoBILOU(prev : Token, token : Token,  next : Token) : String = {
    if(token.nerTag.categoryValue == "O") return "O"
    // The major case that needs to be converted is I, which is dealt with here
    val ts = token.nerTag.categoryValue.split("-")
    var ps : Array[String] = null
    var ns : Array[String] = null
    if(prev != null)
      ps = splitLabel(prev)
    if(next != null)
      ns = splitLabel(next)

    if(token.nerTag.categoryValue.contains("B-")) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      else
        return token.nerTag.categoryValue
    }

    if(prev == null || ps(1) != ts(1)) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      return "B-" + ts(1)
    }
    if(next == null || ns(1) != ts(1) || ns(0) == "B")
      return "L-" + ts(1)
    "I-" + ts(1)
  }

  private def splitLabel(token : Token) : Array[String] = {
    if(token.nerTag.categoryValue.contains("-"))
      token.nerTag.categoryValue.split("-")
    else
      Array("", "O")
  }
}


