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
package cc.factorie
import java.io._

import org.junit.Test

/**
 * User: apassos
 * Date: 10/10/12
 */
class TestExamples {

  val emptyArgs = Array[String]()

  // Returns the name of a new temporary file with the specified
  def dummyFileWithContents(prefix: String, content: String): String = {
    val name = java.io.File.createTempFile("FactorieTestFile", prefix).getAbsolutePath
    val writer = new BufferedWriter(new FileWriter(name))
    writer.write(content)
    writer.close()
    name
  }

  def dummyDirectoryWithFileWithContents(prefix: String, content: String, ext: String = ".txt"): String = {
    val dir = java.io.File.createTempFile("FactorieTestFile", prefix)
    new File(dir.getAbsolutePath + "2").mkdirs()
    val n1 = dir.getAbsolutePath + "2" + java.io.File.separator + "f1" + ext
    val writer = new BufferedWriter(new FileWriter(n1))
    writer.write(content)
    writer.close()
    dir.getAbsolutePath+"2"
  }

  val dummyNERFile = dummyFileWithContents("train", "A NN C I-PER\nA NNS D O\nA NNP C I-LOC")

  @Test def testChainNER1ML() {
    cc.factorie.tutorial.ChainNERExample.main(Array(dummyNERFile, dummyNERFile))
  }

  @Test def testDirichletDemo() {
    cc.factorie.tutorial.DirichletDemo.main(emptyArgs)
  }

  val dummyDir1 = dummyDirectoryWithFileWithContents("documentDir1", "I am a file\n")
  val dummyDir2 = dummyDirectoryWithFileWithContents("documentDir2", "I am a other file\n")

  @Test def testDocumentClassifier1() {
    cc.factorie.tutorial.DocumentClassifier1.main(Array(dummyDir1, dummyDir2))
  }

  val posFile = dummyFileWithContents("POS", "\nHello NN\nWorld NN\n")

  @Test def testForwardBackwardPOS() {
    cc.factorie.tutorial.ForwardBackwardPOS.main(Array("--train", posFile, "--dev", posFile, "--test", posFile))
  }

  @Test def testGaussianDemo() {
    cc.factorie.tutorial.GaussianDemo.main(emptyArgs)
  }

  @Test def testGaussianMixtureDemo() {
    cc.factorie.tutorial.GaussianMixtureDemo.main(emptyArgs)
  }

  @Test def testMultivariateGaussianDemo() {
    cc.factorie.tutorial.MultivariateGaussianDemo.main(emptyArgs)
  }

  @Test def testMultivariateGaussianMixtureDemo() {
    cc.factorie.tutorial.MultivariateGaussianMixtureDemo.main(emptyArgs)
  }

  @Test def testGrid() {
    cc.factorie.tutorial.Grid.main(emptyArgs)
  }

  @Test def testSimpleLDA() {
    cc.factorie.tutorial.SimpleLDA.main(Array(dummyDir1))
  }

  @Test def testEfficientLDA() {
    cc.factorie.tutorial.EfficientLDA.main(Array(dummyDir1))
  }

  @Test def testTopicsOverTime() {
    cc.factorie.tutorial.TopicsOverTime.main(Array(dummyDir1, dummyDir2))
  }

  @Test def testMultinomialDemo() {
    cc.factorie.tutorial.MultinomialDemo.main(emptyArgs)
  }

  @Test def testTutorialVariables() {
    cc.factorie.tutorial.TutorialVariables.main(emptyArgs)
  }

  @Test def testTutorialDomain() {
    cc.factorie.tutorial.TutorialDomain.main(emptyArgs)
  }

  @Test def testTutorialFactors() {
    cc.factorie.tutorial.TutorialFactors.main(emptyArgs)
  }

  @Test def testTutorialFamily() {
    cc.factorie.tutorial.TutorialFamily.main(emptyArgs)
  }

  @Test def testTutorialModel() {
    cc.factorie.tutorial.TutorialModel.main(emptyArgs)
  }

  @Test def testTutorialLearning() {
    cc.factorie.tutorial.TutorialLearning.main(emptyArgs)
  }

  @Test def testTutorialParallelismAndHyperparameters() {
    cc.factorie.tutorial.TutorialParallelismAndHyperparameters.main(emptyArgs)
  }

  @Test def testWordSegmenter() {
    cc.factorie.tutorial.WordSegmenter.main(emptyArgs)
  }
}
