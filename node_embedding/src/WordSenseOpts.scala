import cc.factorie.app.nlp.embeddings.EmbeddingOpts

object WordSenseOpts extends EmbeddingOpts{


  // Model
  val includeEdgeLabels       = new CmdOption("include-edge-labels", true, "BOOLEAN", "Should edges be taken into account when generating vocabulary?")
  val bidirectional           = new CmdOption("bidirectional", false, "BOOLEAN", "Train both ends of edge with context (other, edge)")
  val invertedEdges           = new CmdOption("inverted-edges", true, "BOOLEAN", "If not bidirectional, train concepts with (parent, 'inv'+edge)")
  val combineContext          = new CmdOption("combined-context", false, "BOOLEAN", "Give edge and neighbour to trainer together")

  // IO
  val inputFilename           = new CmdOption("embeddingsfile", "Data/output.csv", "String", "Loads embeddings from this file if not empty")
  val embeddingOutFile        = new CmdOption("embeddingout", "Data/output.csv", "String", "Embeddings are saved to this file if not empty")
  inputFilename.setValue("")
  val corpusses = new CmdOption("corpusses", "", "String", "List all corpusses to be included separated by a ;-sign. " ++
   "The first two corpusses are considered as the corpusses to be aligned. The others are only used for embedding purposes")
  val truthfile = new CmdOption("reference-alignment-name", "Data/anatomy/reference.rdf", "String", "Relative path of the file containing the ground truth")

  // Hard-coding data files
  corpusses.setValue("Data/anatomy/human.owl;Data/anatomy/mouse.owl;Data/NELL_clean.csv")
  output.setValue("Data/output.csv")
  saveVocabFile.setValue("Data/vocab.csv")
  //loadVocabFile.setValue("Data/vocab.csv")
}

