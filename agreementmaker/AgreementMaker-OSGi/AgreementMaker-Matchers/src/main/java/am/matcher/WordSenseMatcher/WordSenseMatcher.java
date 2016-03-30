package am.matcher.WordSenseMatcher;

import am.app.mappingEngine.AbstractMatcher;
import am.app.mappingEngine.Mapping;
import am.app.mappingEngine.similarityMatrix.SimilarityMatrix;
import am.app.ontology.Node;

import java.util.List;

/**
 */
public class WordSenseMatcher extends AbstractMatcher {

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
        return null;
    }


}
