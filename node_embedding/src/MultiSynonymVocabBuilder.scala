/**
  * Created by harmen on 5-6-16.
  */
class MultiSynonymVocabBuilder extends MultiVocabBuilder {

  def addSynonymToVocab(key: String, orig: Int, vid: Int): Unit = {
    val v = vocabs(vid)
    v match {
      case builder: SynonymVocabBuilder =>
        builder.addSynonymToVocab(key, orig)
      case _ => throw new ClassCastException("This vid (%d) does not refer to a SynonymVocabBuilder.".format(vid))
    }
  }
}
