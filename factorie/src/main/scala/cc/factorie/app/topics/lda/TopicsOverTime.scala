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
package cc.factorie.app.topics.lda

import java.io.File
import java.util.Date

import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.directed._
import cc.factorie.variable._

import scala.collection.mutable.ArrayBuffer

// An implementation of Topics-over-Time [Wang, McCallum, KDD 2006]
// TODO Not yet finished; needs the appropriate CollapsedGibbsSampler handler

class TimeStampedDocument(domain:CategoricalSeqDomain[String], name:String, tokens:Seq[String], val date:Date) extends Document(domain, name, tokens)

object TopicsOverTime {
  val numTopics = 10
  implicit val model = DirectedModel()
  implicit val random = new scala.util.Random(0)
  object ZDomain extends DiscreteDomain(numTopics) { type Value = DiscreteValue }
  class Z(value: Int = 0) extends DiscreteVariable(value) { def domain = ZDomain }
  object WordDomain extends CategoricalDomain[String]
  class Word(value: String) extends CategoricalVariable(value) { def domain = WordDomain; def z = model.parentFactor(this).asInstanceOf[CategoricalMixture[String]#Factor]._3 }
  class Document(val file: String) extends ArrayBuffer[Word] {
    var theta: ProportionsVariable = null
    var date: Long = -1  // datetime
    val stamps = new ArrayBuffer[DoubleVariable] // datetime normalized to 0-1
  }
  val beta = MassesVariable.growableUniform(WordDomain, 0.1)
  val alphas = MassesVariable.dense(numTopics, 0.1)
  val phis = Mixture(numTopics)(ProportionsVariable.growableDense(WordDomain) ~ Dirichlet(beta))
  val balphas = Mixture(numTopics)(new DoubleVariable)(model, random)
  val bbetas  = Mixture(numTopics)(new DoubleVariable)(model, random)

  def main(args: Array[String]): Unit = {
    val directories = if (args.length > 0) args.toList else List("/Users/mccallum/research/data/text/nipstxt/nips11")
    val documents = new ArrayBuffer[Document]
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if file.isFile) {
        print("."); Console.flush()
        val doc = new Document(file.toString)
        doc.date = file.lastModified
        doc.theta = ProportionsVariable.dense(numTopics) ~ Dirichlet(alphas)
        for (word <- alphaSegmenter(file).map(_.toLowerCase).filter(!StopWords.contains(_))) {
          val z = new Z :~ Discrete(doc.theta)
          val w = new Word(word)
          CategoricalMixture.newFactor(w, phis, z)
          doc += w
          doc.stamps += new DoubleVariable ~ BetaMixture(balphas, bbetas, z)
        }
        documents += doc
      }
    }
    println("\nRead "+documents.size+" documents, containing "+documents.map(_.size).sum+" tokens.")
    
    // Now that we have the full min-max range of dates, set the doc.stamps values to a 0-1 normalized value
    val dates = documents.map(_.date)
    val maxDate = dates.max
    val minDate = dates.min
    val dateRange: Double = maxDate - minDate
    documents.foreach(doc => doc.stamps.foreach(_ := (doc.date - minDate) / dateRange)) 

    val collapse = new ArrayBuffer[Var]
    collapse += phis
    collapse ++= documents.map(_.theta)
    val sampler = new CollapsedGibbsSampler(collapse, model)
    val startTime = System.currentTimeMillis
    for (i <- 1 to 20) {
      //throw new Error("Not yet a GibbsSampler handler for this case.")
      for (doc <- documents; word <- doc) sampler.process(word.z)
      if (i % 5 == 0) {
        println("Iteration " + i)
        // Turned off hyperparameter optimization
        //DirichletMomentMatching.estimate(alphaMean, alphaPrecision)
        //println("alpha = " + alphaMean.map(_ * alphaPrecision.doubleValue).mkString(" "))
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.value.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")))
        println()
      }
      // Re-estimate the time stamp Beta parameters
      balphas.zip(bbetas).foreach(alphabeta => MaximizeBetaByMomentMatching(alphabeta._1, alphabeta._2, model))
      balphas.zip(bbetas).foreach(alphabeta => println("alpha="+alphabeta._1.doubleValue+" beta="+alphabeta._2.doubleValue))
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")

  }

}