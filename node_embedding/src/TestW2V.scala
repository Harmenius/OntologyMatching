/**
  * Created by harmen on 7-5-16.
  */
object TestW2V {

  def main(args: Array[String]) {
    val model = new SkipGramNodeEmbedding
    model.buildVocab()
    model.learnEmbeddings()
    val vocab = model.getVocab
    for (i <- 0 until 10) {
      val k = 5
      val id = vocab.getRandWordId()
      val word = vocab.getWord(id)
      val knn = model.getKNN(model.getVector(id), k)
      val knnames = knn.map(tup => vocab.getWord(tup._2))
      printf("Word: %s%n Top-%d synonyms:%n%s%n", word, k, knnames.mkString(", "))
    }

  }

}
