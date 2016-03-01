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
package cc.factorie.optimize

import cc.factorie.app.classify.backend._
import cc.factorie.la.{SingletonTensor1, SparseIndexedTensor1, Tensor1}
import cc.factorie.traversableExtras
import cc.factorie.util.BinarySerializer
import cc.factorie.util.CubbieConversions._
import cc.factorie.variable.{DiscreteDomain, VectorDomain}
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class TestDecisionTree extends JUnitSuite {
  @Test def testRegression(): Unit = {
    implicit val random = new scala.util.Random(0)
    val xs = Seq[Double](1, 2, 6, 8, 34, 45)
    val ys = xs.map(x => x * x)
    xs.zip(ys).foreach({case (x, y) => println(f"x: $x%f y: $y%f")})
    val instances = xs.zip(ys).map({
      case (x, y) => DecisionTreeTrainer.Instance(new SingletonTensor1(1, 0, x), new SingletonTensor1(1, 0, y), 1.0)
    })
    val trainer = new RegressionTreeTrainer { minSampleSize = 1 }
    val tree = trainer.train(instances)
    val xs2 = Seq[Double](5, 7, 8, 23, 50)
    val preds = xs2.map(x => DTree.score(new SingletonTensor1(1, 0, x), tree)(0))
    xs2.zip(preds).foreach({
      case (x, y) => println(f"x: $x%f y: $y%f")
    })
    assert(preds.sameElements(Seq[Double](36, 36, 64, 1156, 2025)), "Dec tree regression didn't work!")
  }
  @Test def testRegression2(): Unit = {
    // separate test to split real variables with negative values
    implicit val random = new scala.util.Random(0)
    val xs = Seq[Double](-10, -5, 1, 2, 6, 8)
    val ys = xs.map(x => x * x * x)
    xs.zip(ys).foreach({case (x, y) => println(f"x: $x%f y: $y%f")})
    val instances = xs.zip(ys).map({
      case (x, y) => DecisionTreeTrainer.Instance(new SingletonTensor1(1, 0, x), new SingletonTensor1(1, 0, y), 1.0)
    })
    val trainer = new RegressionTreeTrainer { minSampleSize = 1 }
    val tree = trainer.train(instances)
    val xs2 = Seq[Double](-8, 5, 7, 9)
    val preds = xs2.map(x => DTree.score(new SingletonTensor1(1, 0, x), tree)(0))
    xs2.zip(preds).foreach({
      case (x, y) => println(f"x: $x%f y: $y%f")
    })
    assert(preds.sameElements(Seq[Double](-1000, 216, 216, 512)), "Dec tree regression didn't work!")
  }
  @Test def testClassification(): Unit = {
    implicit val random = new scala.util.Random(0)
    object featuresDomain extends VectorDomain {
      val dimensionDomain = new DiscreteDomain(100)
    }
    object labelDomain extends DiscreteDomain(2)
    val mean1 = (0 until 100).map(_ => random.nextDouble()).toSeq
    val mean2 = (0 until 100).map(_ => random.nextDouble()).toSeq
    val positiveExampleSeqs = (0 until 100).map(_ => (0 until 10).map(_ => mean1.zipWithIndex.sampleProportionally(_._1)._2))
    val negativeExampleSeqs = (0 until 100).map(_ => (0 until 10).map(_ => mean2.zipWithIndex.sampleProportionally(_._1)._2))
    val posExampleTensors = positiveExampleSeqs.map(pos => {
      val t = new SparseIndexedTensor1(100)
      pos.foreach(p => t += (p, 1.0))
      t
    })
    val negExampleTensors = negativeExampleSeqs.map(neg => {
      val t = new SparseIndexedTensor1(100)
      neg.foreach(p => t += (p, 1.0))
      t
    })

    // add truth feature - if this doesn't help, we have a bug
    // TODO add some tests for feature splitting
//    posExampleTensors.foreach(t => t += (100, 1.0))
    val (trainSet, testSet) = (posExampleTensors.map(p => (p, 1)) ++ negExampleTensors.map(n => (n, 0))).shuffle.split(0.5)
    val trainers = Seq(
      new BoostingMulticlassTrainer(100),
      new OnlineLinearMulticlassTrainer,
      new RandomForestMulticlassTrainer(100, 100, 100),
      new DecisionTreeMulticlassTrainer(new C45DecisionTreeTrainer))

    val trainFeatures = trainSet.map(_._1)
    val trainLabels = trainSet.map(_._2)
    val testFeatures = testSet.map(_._1)
    val testLabels = testSet.map(_._2)
    def calcAccuracy(c: MulticlassClassifier[Tensor1]): Double =
      testFeatures.map(i => c.classification(i).bestLabelIndex)
        .zip(testLabels).count(i => i._1 == i._2).toDouble/testLabels.length
    val evaluate = (c: MulticlassClassifier[Tensor1]) => {
      val accuracy = calcAccuracy(c)
      println(f"Test accuracy: $accuracy%1.4f")
      assert(accuracy > 0.66)
    }
    val evaluate2 = (c1: MulticlassClassifier[Tensor1], c2: MulticlassClassifier[Tensor1]) => {
      val accuracy1 = calcAccuracy(c1)
      val accuracy2 = calcAccuracy(c2)
      println(f"Test accuracy: $accuracy1%1.4f")
      assert(accuracy1 > 0.66 && accuracy1 == accuracy2)
    }

    for (trainer <- trainers)
      trainer.simpleTrain(2, 100, trainLabels, trainFeatures, trainSet.map(_ => 1.0), evaluate)

    // confirm i can serialize dec trees
    val file = java.io.File.createTempFile("FactorieTestFile", "serialize-randomforest").getAbsolutePath

    println("Testing deserialized:")

    val rfc = new RandomForestMulticlassTrainer(100, 100, 100).simpleTrain(2, 100, trainLabels, trainFeatures, trainSet.map(_ => 1.0), _ => {})
    BinarySerializer.serialize(rfc, file)
    val rfc2 = BinarySerializer.deserialize[RandomForestMulticlassClassifier](file)

    evaluate2(rfc, rfc2)

    val bc = new BoostingMulticlassTrainer(100).simpleTrain(2, 100, trainLabels, trainFeatures, trainSet.map(_ => 1.0), _ => {})
    BinarySerializer.serialize(bc, file)
    val bc2 = BinarySerializer.deserialize[BoostedMulticlassClassifier](file)

    evaluate2(bc, bc2)
  }
}
