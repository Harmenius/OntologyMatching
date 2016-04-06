package am.matcher.WordSenseMatcher;

import am.app.mappingEngine.AbstractMatcherParametersPanel;
import am.app.mappingEngine.DefaultMatcherParameters;

public class WordSenseParametersPanel extends AbstractMatcherParametersPanel {

    @Override
    public DefaultMatcherParameters getParameters() {
        return new WordSenseParameters();
    }
}
