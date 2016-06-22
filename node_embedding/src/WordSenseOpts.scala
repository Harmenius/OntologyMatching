import cc.factorie.app.nlp.embeddings.EmbeddingOpts

object WordSenseOpts extends EmbeddingOpts{


  minCount.setValue(0) // For some reason many nodes have no neighbours. Looking into that.
  // Model
  val includeEdgeLabels = new CmdOption("include-edge-labels", true, "BOOLEAN", "Should edges be taken into account when generating vocabulary?")
  val bidirectional     = new CmdOption("bidirectional", false, "BOOLEAN", "Train both ends of edge with context (other, edge)")
  val invertedEdges     = new CmdOption("inverted-edges", true, "BOOLEAN", "If not bidirectional, train concepts with (parent, 'inv'+edge)")
  val combineContext    = new CmdOption("combined-context", false, "BOOLEAN", "Give edge and neighbour to trainer together")
  val synonyms          = new CmdOption("synonym-file", "Data/anatomy/VectorMatcherResults.rdf", "String", "File containing synonyms")
  val nIts              = new CmdOption("n-its", 5, "INTEGER", "Number of times w2v should go over the corpus")
  val continue          = new CmdOption("continue", true, "BOOLEAN", "")
  nIts.setValue(0)//1000000)
  threads.setValue(4)
  continue.setValue(false)
  dimension.setValue(50)

  // IO
  val inputFilename     = new CmdOption("embeddingsfile", "", "String", "Loads embeddings from this file if not empty")
  val embeddingOutFile  = new CmdOption("embeddingout", "Data/output.csv", "String", "Embeddings are saved to this file if not empty")
  val corpusses         = new CmdOption("corpusses", "", "String", "List all corpusses to be included separated by a ;-sign. " ++
   "The first two corpusses are considered as the corpusses to be aligned. The others are only used for embedding purposes")
  val truthfile         = new CmdOption("reference-alignment-name", "Data/anatomy/reference.rdf", "String", "Relative path of the file containing the ground truth")

  // Hard-coding data files
  //corpusses.setValue("Data/anatomy/mouse.owl;Data/anatomy/human.owl;Data/NELL_clean.csv")
  corpusses.setValue("Data/anatomy/mouse.owl;Data/anatomy/human.owl")
  //corpusses.setValue("Data/medical/oaei2013_SNOMED_small_overlapping_nci.owl;Data/medical/oaei2013_NCI_small_overlapping_snomed.owl")
  //truthfile.setValue("Data/medical/oaei2013_SNOMED2NCI_repaired_UMLS_mappings.rdf")
  output.setValue("Data/output.csv")
  //saveVocabFile.setValue("Data/vocab.csv")
  //loadVocabFile.setValue("Data/vocab.csv")
  //inputFilename.setValue("Data/output.csv")
  //inputFilename.setValue("Data/forthelongesttime.csv")
  //inputFilename.setValue("edgemappings.csv")
  //inputFilename.setValue("Data/pretrained/glove.6B.50d.txt")
  continue.setValue(continue.value && !inputFilename.value.isEmpty) // Can't continue on nothing
}

