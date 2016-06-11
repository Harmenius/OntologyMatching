/**
  * Created by harmen on 5-6-16.
  */
class MultiSynonymVocabBuilder(n: Int = 2) extends MultiVocabBuilder(n) {

  override val vocabs: List[SynonymVocabBuilder] = List.fill(n)(new SynonymVocabBuilder())

  def addSynonymToVocab(key: String, orig: Int, vid: Int): Unit = {
    val v = vocabs(vid)
    v match {
      case builder: SynonymVocabBuilder =>
        builder.addSynonymToVocab(key, orig)
      case _ => throw new ClassCastException("This vid (%d) does not refer to a SynonymVocabBuilder.".format(vid))
    }
  }

  /**
    * Does something different than usual!
    * Only applies min_count to the third vocab because that is what my implementation required.
    * @param min_count Remove entries with fewer than min_count occurrences.
    * @param ignoreStopWords Removes words that occur too often.
    * @param max_vocab_size Removes more words if there are too many.
    */
  override def sortVocab(min_count: Int = 5, ignoreStopWords: Int = 1, max_vocab_size: Int = 2e6.toInt) : Unit = {
    for ((v, i) <- vocabs.zipWithIndex) {
      if (i==3)
        sortVocab(min_count, ignoreStopWords, max_vocab_size)
      else
        sortVocab(0, ignoreStopWords, max_vocab_size)
    }
  }

}
