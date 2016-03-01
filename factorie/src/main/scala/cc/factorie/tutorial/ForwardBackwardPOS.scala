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
package cc.factorie.tutorial

import java.io.File

import cc.factorie._
import cc.factorie.app.chain.ChainModel
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load.LoadOWPL
import cc.factorie.app.nlp.pos.{LabeledPennPosTag, PennPosDomain, PennPosTag}
import cc.factorie.optimize.Trainer
import cc.factorie.util.BinarySerializer
import cc.factorie.variable.{BinaryFeatureVectorVariable, CategoricalVectorDomain, LabeledVar}

/**
 * Author: martin
 * Date: 2/8/12
 *
 * A simple chain POS tagger using the ChainModel's examples and inference.
 */

/** A simple demonstration of part-of-speech tagging with a finite-state linear-chain CRF.
    The set of features is impoverished in this demonstration, so the accuracy is not high */
object ForwardBackwardPOS {
  
  object PosFeaturesDomain extends CategoricalVectorDomain[String]
  class PosFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] { def domain = PosFeaturesDomain }

  object PosModel extends ChainModel[LabeledPennPosTag,PosFeatures,Token](PennPosDomain, PosFeaturesDomain, l => l.token.attr[PosFeatures], l => l.token, t => t.attr[LabeledPennPosTag])

  def initPosFeatures(documents: Seq[Document]): Unit = documents.map(initPosFeatures)
  def initPosFeatures(document: Document): Unit = {
    for (token <- document.tokens) {
      val rawWord = token.string
      val word = cc.factorie.app.strings.simplifyDigits(rawWord)
      val features = new PosFeatures(token)
      // val features = token.attr += new PosFeatures(token)
      token.attr += features
      features += "W=" + word
      features += "SHAPE3=" + cc.factorie.app.strings.stringShape(rawWord, 3)
      val i = 3
      features += "SUFFIX" + i + "=" + word.takeRight(i)
      features += "PREFIX" + i + "=" + word.take(i)
      if (token.isCapitalized) features += "CAPITALIZED"
      if (token.string.matches("[A-Z]")) features += "CONTAINS_CAPITAL"
      if (token.string.matches("-")) features += "CONTAINS_DASH"
      if (token.containsDigit) features += "NUMERIC"
      if (token.isPunctuation) features += "PUNCTUATION"
    }
    for (sentence <- document.sentences)
      addNeighboringFeatureConjunctions(sentence.tokens, (t: Token) => t.attr[PosFeatures], "W=", List(-2), List(-1), List(1), List(-2,-1), List(-1,0))
  }

  def percentageSetToTarget[L <: LabeledVar](ls: Seq[L]): Double = {
    val numCorrect = ls.foldLeft(0.0)((partialSum, label) => partialSum + {if (label.valueIsTarget) 1 else 0})
    numCorrect / ls.size * 100
  }

  def predictSentence(s: Sentence): Unit = predictSentence(s.tokens.map(_.attr[LabeledPennPosTag]))
  def predictSentence(vs: Seq[LabeledPennPosTag], oldBp: Boolean = false): Unit =
    PosModel.maximize(vs)(null)

  def train(
        documents: Seq[Document],
        devDocuments: Seq[Document],
        testDocuments: Seq[Document] = Seq.empty[Document],
        iterations: Int = 100,
        modelFile: String = "",
        extraId: String = "")(implicit random: scala.util.Random): Unit = {

    def testSavePrint(label: String): Unit = {
      test(documents, label = "train")
      if (testDocuments.nonEmpty)
        test(testDocuments, label = "test")
      if (devDocuments.nonEmpty)
        test(devDocuments, label = "dev")
      if (modelFile != "")
        BinarySerializer.serialize(PosFeaturesDomain, PosModel, new File(modelFile + label + extraId), gzip = true)
    }

    val sentences: Seq[Sentence] = documents.flatMap(_.sentences)
    val sentenceTags = sentences.map(_.posTags).filter(_.size > 0)

    val examples = sentenceTags.map(s => new PosModel.ChainLikelihoodExample(s.asInstanceOf[Seq[LabeledPennPosTag]]))
    Trainer.onlineTrain(PosModel.parameters, examples, maxIterations=10)
    testSavePrint("final")
  }

  def test(documents: Seq[Document], label: String = "test"): Unit = {
    implicit val random = new scala.util.Random(0)
    val sentences = documents.flatMap(_.sentences)
    val labels = sentences.map(_.tokens).flatMap(_.map(t => t.attr[LabeledPennPosTag]))
    labels.map(_.setRandomly)
    sentences.map(predictSentence)
    println(label + " accuracy: " + percentageSetToTarget(labels) + "%")
  }

  var modelLoaded = false
  def load(modelFile: String) = { BinarySerializer.deserialize(PosFeaturesDomain, PosModel, new File(modelFile), gzip = true); modelLoaded = true }

  def process(documents: Seq[Document]): Unit = documents.map(process)
  def process(document: Document): Unit = {
    if (!modelLoaded) throw new Error("The model should be loaded before documents are processed.")

    // add the labels and features if they aren't there already.
    if (document.tokens.head.attr.get[PennPosTag] == None) {
      document.tokens.foreach(t => t.attr += labelMaker(t)(0))
      initPosFeatures(document)
    }

    document.sentences.foreach(predictSentence)
  }

  lazy val defaultCategory = {
    try { PennPosDomain.categories.head }
    catch { case e: NoSuchElementException => throw new Error("The domain must be loaded before it is accessed.") }
  }
  def labelMaker(t: Token, l: Seq[String] = Seq(defaultCategory)) = Seq(new LabeledPennPosTag(t, l(0)))

  def main(args: Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val trainFile = new CmdOption("train", "", "FILE", "An OWPL train file.", true)
      val devFile =   new CmdOption("dev", "", "FILE", "An OWPL dev file", true)
      val testFile =  new CmdOption("test", "", "FILE", "An OWPL test file.", true)
      val takeOnly =  new CmdOption("takeOnly", -1, "INT", "A limit on the number of sentences loaded from each file.")
      val iterations =new CmdOption("iterations", 10, "INT", "The number of iterations to train for.")
      val modelDir =  new CmdOption("model", "", "DIR", "Directory in which to save the trained model.")
      val extraId =   new CmdOption("label", "", "STRING", "An extra identifier.  Useful for testing different sets of features.")
    }
    opts.parse(args)
    implicit val random = new scala.util.Random(0)
    import opts._

    if (trainFile.wasInvoked && devFile.wasInvoked && testFile.wasInvoked) {
      def load(f: String): Seq[Document] = LoadOWPL.fromFilename(f, labelMaker, limitSentenceCount=takeOnly.value)

      // load the data
      val trainDocs = load(trainFile.value)
      val devDocs = load(devFile.value)
      val testDocs = load(testFile.value)

      initPosFeatures(trainDocs ++ devDocs ++ testDocs)

      train(trainDocs, devDocs, testDocs,
        iterations = iterations.value,
        modelFile = modelDir.value,
        extraId = extraId.value
      )
    }
  }

}
