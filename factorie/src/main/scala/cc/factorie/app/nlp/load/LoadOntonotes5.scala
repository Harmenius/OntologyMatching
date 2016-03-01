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

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.pos.LabeledPennPosTag

import scala.io.Source

/* Loader for the OntoNotes 5 data
   @author Brian Martin, Andrew McCallum
  1   token ID
  2   word form
  3   auto lemma
  4   gold lemma
  5   auto POS tag
  6   gold POS tag
  7   auto feats
  8	  gold feats
  9   auto head ID				
  10  gold head ID
  11  auto dependency label		
  12  gold dependency label
  13  gold secondary dependencies
  14  gold semantic arguments
  15  gold named entity tags
  16  gold coreference

 */

sealed trait AnnotationType
case object DoNotLoad extends AnnotationType
case object GoldLabel extends AnnotationType
case object AutoLabel extends AnnotationType

object LoadOntonotes5 extends Load {
  private def addDepInfo(s: Sentence, depInfoSeq: Seq[(Int,Int,String)]): Unit = {
    //assert(depInfoSeq.map(_._1) == Seq.tabulate(depInfoSeq.length)(i => i), "Token indices: "+depInfoSeq.map(_._1).mkString(" ")) // Assert that we have an entry for each token index, in order
    val tree = new ParseTree(s, depInfoSeq.map(_._2), depInfoSeq.map(_._3))
    s.attr += tree
  }

  def fromSource(source: Source) =
    fromSource(source, filename="?UNKNOWN?", loadLemma=GoldLabel, loadPos=GoldLabel, loadParse=GoldLabel, loadNer=true, nerBilou=true)

  def fromSource(source: Source, filename:String, loadLemma:AnnotationType, loadPos:AnnotationType, loadParse:AnnotationType, loadNer:Boolean, nerBilou:Boolean): Seq[Document] = {
    val lines = source.getLines()
    val document: Document = new Document().setName("Ontonotes499/" + filename)
    document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    if (loadPos != DoNotLoad) document.annotators(classOf[pos.PennPosTag]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
    if (loadNer) if (nerBilou) document.annotators(classOf[ner.BilouOntonotesNerTag]) = UnknownDocumentAnnotator.getClass else document.annotators(classOf[ner.BioOntonotesNerTag]) = UnknownDocumentAnnotator.getClass
    var sentence: Sentence = new Sentence(document)
    var depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
    for (line <- lines) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addDepInfo(sentence, depInfoSeq)
        depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        sentence = null
      } else {
        if (sentence eq null)
          sentence = new Sentence(document) // avoids empty sentence at the end of doc
        val fields = line.split('\t')
        assert(fields.length >= 10, "Fewer than 10 fields in file "+filename+"\nOffending line:\n"+line)

        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)
        
        val autoLemma = fields(2)
        val goldLemma = fields(3)
        
        val autoPartOfSpeech = fields(4)
        val goldPartOfSpeech = fields(5)
        
        // OFF BY 1!
        val autoParentIdx = fields(8).toInt - 1 
        val goldParentIdx = fields(9).toInt - 1
        
        val autoDepLabel = fields(10)
        val goldDepLabel = fields(11)
        
        var ner = fields(14); if (ner == "_") ner = "O"  // If we wanted to distinguish "unnamed entities" from background, we wouldn't have this.
        
        document.appendString(" ")
        val token = new Token(sentence, word)
        loadPos match {
	      case GoldLabel => {token.attr += new LabeledPennPosTag(token, if (goldPartOfSpeech == "XX") "PUNC" else goldPartOfSpeech)}
	      case AutoLabel => {token.attr += new LabeledPennPosTag(token, if (autoPartOfSpeech == "XX") "PUNC" else autoPartOfSpeech)}
	      case DoNotLoad => {/* do nothing */}
        }
        loadLemma match {
	      case GoldLabel => {token.attr += new TokenLemma(token, goldLemma)}
	      case AutoLabel => {token.attr += new TokenLemma(token, autoLemma)}
	      case DoNotLoad => {/* do nothing */}
        }
        loadParse match {
	      case GoldLabel => {depInfoSeq.append((currTokenIdx, goldParentIdx, goldDepLabel))}
	      case AutoLabel => {depInfoSeq.append((currTokenIdx, autoParentIdx, autoDepLabel))}
	      case DoNotLoad => {/* do nothing */}
        }
        if (loadNer) token.attr += (if (nerBilou) new LabeledBilouOntonotesNerTag(token, ner) else new LabeledBioOntonotesNerTag(token, ner))
      }
    }
    if ((sentence != null) && (loadParse != DoNotLoad)) addDepInfo(sentence, depInfoSeq)
    if (nerBilou) convertBioBilou(document.asSection)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.asSection.length+" tokens total from file "+filename)
    Seq(document)
  }

  def fromFilename(filename:String): Seq[Document] = {
    fromFilename(filename, GoldLabel, GoldLabel, GoldLabel, true, true)
  }

  def fromFilename(filename:String, loadLemma:AnnotationType, loadPos:AnnotationType, loadParse:AnnotationType, loadNer:Boolean, nerBilou:Boolean): Seq[Document] =
    fromSource(Source.fromFile(filename), filename, loadLemma, loadPos, loadParse, loadNer, nerBilou)

  def convertBioBilou(section:Section): Unit = {
    /** Return the string of the NER label, including the two letter (B- or I-) prefix. */
    def cat(token:Token): String = if (token eq null) "null" else token.attr[BilouOntonotesNerTag].categoryValue
    /** Return true if the strings are equal without their two letter (B- or I-) prefix. */
    def sim(s1:String, s2:String): Boolean = s1.drop(2) == s2.drop(2)
    def isU(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && (!sim(cat2, cat3) || cat3(0) == 'B')
    def isB(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && sim(cat2, cat3) && cat3(0) == 'I'
    def isL(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && sim(cat1, cat2) && (cat3(0) == 'B' || !sim(cat2, cat3))
    def isI(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && cat3(0) == 'I'
    for (token <- section.tokens) if (token.attr[LabeledBilouOntonotesNerTag].intValue != 0) {
      val nerLabel = token.attr[LabeledBilouOntonotesNerTag]
      val cat1 = cat(token.prev); val cat2 = cat(token); val cat3 = cat(token.next)
      if (isU(cat1, cat2, cat3)) nerLabel.target.setCategory("U-"+cat2.drop(2))(null)
      else if (isB(cat1, cat2, cat3)) nerLabel.target.setCategory("B-"+cat2.drop(2))(null)
      else if (isL(cat1, cat2, cat3)) nerLabel.target.setCategory("L-"+cat2.drop(2))(null)
      else if (isI(cat1, cat2, cat3)) nerLabel.target.setCategory("I-"+cat2.drop(2))(null)
      nerLabel.setToTarget(null)
    }
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)
}
