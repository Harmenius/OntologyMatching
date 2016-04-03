package am.matcher.WordSenseMatcher;

import am.app.mappingEngine.AbstractMatcher;
import am.app.mappingEngine.Mapping;
import am.app.mappingEngine.similarityMatrix.SimilarityMatrix;
import am.app.ontology.Node;

import java.util.List;

/**
 */
public class WordSenseMatcher extends AbstractMatcher {

    SkipGramNodeEmbedding model = new SkipGramNodeEmbedding(null);
    double threshold = 100.0;

    public WordSenseMatcher() {
        super();
        setName("WordSenseMatcher");
    }

    public String getDescriptionString() {
        return "Uses distributed representations of concepts as created by the word2vec model\n" +
                "as a distance measure between nodes. The model is pretrained but also internalizes\n" +
                "the two ontologies and uses a nodes neighborhood as conext.\n" +
                "If an alignment is given, the semantic space will be warped accordingly to improve matching.";
    }

    @Override
    protected SimilarityMatrix alignNodesOneByOne( List<Node> sourceList, List<Node> targetList, alignType typeOfNodes) throws Exception {
        SkipGramNodeEmbedding model = new SkipGramNodeEmbedding(null);
        model.opts().corpus().setValue("Data/corpus.csv");
        model.buildVocab();
        model.learnEmbeddings();
        // Run source and target through w2v
        return super.alignNodesOneByOne(sourceList, targetList, typeOfNodes);
    }

    /**This is the main method that is overridden
     * @see am.app.mappingEngine.AbstractMatcher#alignTwoNodes
     * @param source the source concept
     * @param target the target concept
     * @param typeOfNodes can be alignType.alignClasses or alignType.aligningProperties, and it tells you if you in the alignProperties or classes function
     * @return the alignment between the two nodes (a, b, sim, relation)
     * @throws Exception are managed in the doInBackground() method, to interrupt the process to send a message to the user thow new AMException(MESSAGE)
     */
    @Override
    protected Mapping alignTwoNodes(Node source, Node target, alignType typeOfNodes, SimilarityMatrix matrix ) throws Exception {
        String source_label = source.getLabel();              String target_label = target.getLabel();
        int source_index = model.vocab().getId(source_label); int target_index = model.vocab().getId(target_label);
        double[] source_vec = model.getVector(source_index);  double[] target_vec = model.getVector(target_index);
        double d = 0;
        for (int i = 0; i<= source_vec.length; i++) {
            d += Math.pow((source_vec[i] - target_vec[i]), 2);
        }
        d = Math.sqrt(d);
        if (d>threshold)
            return null;
        else
            return new Mapping(source, target);
    }


}
