package am.matcher.WordSenseMatcher;

import am.Utility;
import am.app.mappingEngine.*;
import am.app.mappingEngine.referenceAlignment.ReferenceAlignmentMatcher;
import am.app.mappingEngine.referenceAlignment.ReferenceAlignmentParameters;
import am.app.mappingEngine.referenceAlignment.ReferenceEvaluator;
import am.app.ontology.Ontology;
import am.app.ontology.ontologyParser.OntoTreeBuilder;
import am.app.ontology.ontologyParser.OntologyDefinition;

import java.util.ArrayList;

public class EvaluateMain {

    private static WordSenseMatcher mm;

    public static void main(String[] args) throws Exception {

        String ONTOLOGY_BASE_PATH ="conference_dataset/"; // Use your base path
        String[] confs = {"cmt","conference","confOf","edas","ekaw","iasted","sigkdd"};
        //String[] confs = {"edas","sigkdd"};


        mm = new WordSenseMatcher();
        System.setProperty("wordnet.database.dir","wordnet-3.0/dict");
        double precision=0.0d;
        double recall=0.0d;
        double fmeasure=0.0d;
        int size=21;

        //stop=new StopWords();
        for(int i = 0; i < confs.length-1; i++)
        {
            for(int j = i+1; j < confs.length; j++)
            {
                Ontology source = OntoTreeBuilder.loadOWLOntology(ONTOLOGY_BASE_PATH + "/"+confs[i]+".owl");
                Ontology target = OntoTreeBuilder.loadOWLOntology(ONTOLOGY_BASE_PATH + "/"+confs[j]+".owl");

                OntologyDefinition def1=new OntologyDefinition(true, source.getURI(), null, null);
                OntologyDefinition def2=new OntologyDefinition(true, target.getURI(), null, null);

                def1.largeOntologyMode=false;
                source.setDefinition(def1);
                def2.largeOntologyMode=false;
                target.setDefinition(def2);
                mm.setSourceOntology(source);
                mm.setTargetOntology(target);

                DefaultMatcherParameters param = new DefaultMatcherParameters();

                //Set your parameters
                param.threshold = 0.0;
                param.maxSourceAlign = 1;
                param.maxTargetAlign = 1;
                //	mm.setName(TARGET_ONTOLOGY);
                mm.setParameters(param);

                try {
                    mm.match();

                } catch (Exception e) {
                    e.printStackTrace();
                }

				/*writer=new FileOutput(confs[i]+"-"+confs[j]+".csv");
				writer.writeHeader();*/
                ArrayList<Double> results=	referenceEvaluation(ONTOLOGY_BASE_PATH + confs[i]+"-"+confs[j]+".rdf");
                //Add fscore to a list
                precision+=results.get(0);
                recall+=results.get(1);
                fmeasure+=results.get(2);
			/*writer.close();*/


            }

        }

        StringBuilder sb= new StringBuilder();
        precision/=size;
        recall/=size;
        fmeasure/=size;

        String pPercent = Utility.getOneDecimalPercentFromDouble(precision);
        String rPercent = Utility.getOneDecimalPercentFromDouble(recall);
        String fPercent = Utility.getOneDecimalPercentFromDouble(fmeasure);


        sb.append("Precision = Correct/Discovered: "+ pPercent+"\n");
        sb.append("Recall = Correct/Reference: "+ rPercent+"\n");
        sb.append("Fmeasure = 2(precision*recall)/(precision+recall): "+ fPercent+"\n");


        String report=sb.toString();
        System.out.println("Evaulation results:");
        System.out.println(report);

    }


    private static ArrayList<Double> referenceEvaluation(String pathToReferenceAlignment)
            throws Exception {


        // Run the reference alignment matcher to get the list of mappings in
        // the reference alignment file
        ReferenceAlignmentMatcher refMatcher = new ReferenceAlignmentMatcher();


        // these parameters are equivalent to the ones in the graphical
        // interface
        ReferenceAlignmentParameters parameters = new ReferenceAlignmentParameters();
        parameters.fileName = pathToReferenceAlignment;
        parameters.format = ReferenceAlignmentMatcher.OAEI;
        parameters.onlyEquivalence = false;
        parameters.skipClasses = false;
        parameters.skipProperties = false;
        refMatcher.setSourceOntology(mm.getSourceOntology());
        refMatcher.setTargetOntology(mm.getTargetOntology());

        // When working with sub-superclass relations the cardinality is always
        // ANY to ANY
        if (!parameters.onlyEquivalence) {
            parameters.maxSourceAlign = AbstractMatcher.ANY_INT;
            parameters.maxTargetAlign = AbstractMatcher.ANY_INT;
        }

        refMatcher.setParam(parameters);

        // load the reference alignment
        refMatcher.match();

        Alignment<Mapping> referenceSet;
        if (refMatcher.areClassesAligned() && refMatcher.arePropertiesAligned()) {
            referenceSet = refMatcher.getAlignment(); // class + properties
        } else if (refMatcher.areClassesAligned()) {
            referenceSet = refMatcher.getClassAlignmentSet();
        } else if (refMatcher.arePropertiesAligned()) {
            referenceSet = refMatcher.getPropertyAlignmentSet();
        } else {
            // empty set? -- this should not happen
            referenceSet = new Alignment<Mapping>(Ontology.ID_NONE,
                    Ontology.ID_NONE);
        }

        // the alignment which we will evaluate
        Alignment<Mapping> myAlignment;

        myAlignment = mm.getAlignment();

        // use the ReferenceEvaluator to actually compute the metrics
        ReferenceEvaluationData rd = ReferenceEvaluator.compare(myAlignment,
                referenceSet);

        // output the report
        StringBuilder report = new StringBuilder();
        report.append("Reference Evaluation Complete\n\n").append(mm.getName())
                .append("\n\n").append(rd.getReport()).append("\n");

        double precision=rd.getPrecision();
        double recall=rd.getRecall();
        double fmeasure=rd.getFmeasure();

        ArrayList<Double> results=new ArrayList<Double>();
        results.add(precision);
        results.add(recall);
        results.add(fmeasure);

        return results;

        // use system out if you don't see the log4j output
        //	System.out.println(report);

    }

}
