package am.matcher.WordSenseMatcher

import scala.util.control.Breaks.break
import cc.factorie.app.nlp.embeddings.{SkipGramNegSamplingExample, EmbeddingOpts}

class SkipGramNodeEmbedding(override val opts: EmbeddingOpts) extends NodeEmbeddingModel(opts){
  val negative = opts.negative.value
  val window = opts.window.value
  val rng = new util.Random(5) // https://github.com/iesl/Word2Vec.git
  val sample = opts.sample.value.toDouble
  override def process(doc: String): Int = {
    val words = doc.split(' ')
    val concepts = Array(words(0), words(2)).map(concept => vocab.getId(concept.toLowerCase))
    val conceptCount = 2

    for (i <- 0 until 2) {
      if (concepts(i) != -1) {
        var context = new collection.mutable.ArrayBuffer[Int]
        context += concepts(1 - i) // Add the other concept
        if (false)//bidirectional)
          println("Bidirectionality not implemented yet") //TODO: implement bidirectionality
        else if (false)//addInvertedEdges)
          println("Inverted edges not implemented yet") //TODO: implement inverted edges
        else if (i == 1) // Otherwise parents are not in context so no training required
          break

        context = context.filter(v => v != -1)
        if (false)//combineContext)
          println("Combined context not implemented yet") //TODO: implement context combining
        else
          context.foreach(context => {
            trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), context, 1))
            (0 until negative).foreach(neg => trainer.processExample(new SkipGramNegSamplingExample(this, concepts(i), vocab.getRandWordId, -1)))
          })
      }
    }
    return conceptCount
  }

  def getVector(index: Int) : Array[Double] = {
    val weight = weights(index)
    val output = Array[Double](weight.value.size)
    for (i <- weight.value.size) output(i) = weight.value(i)
    return output
  }

}
