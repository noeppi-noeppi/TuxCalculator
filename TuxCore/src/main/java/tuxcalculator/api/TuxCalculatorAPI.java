package tuxcalculator.api;

import java.io.InputStream;
import java.nio.file.Path;

/**
 * Public API for interfacing with TuxCalculator
 */
public interface TuxCalculatorAPI {

    String VERSION = "1.1.7";

    /**
     * Create a new ini-mode calculator.
     */
    TuxCalculator createINI(TuxFrontend frontend);

    /**
     * Create a new calculator using the bundled plain format.
     */
    TuxCalculator.Builder createPlain(TuxFrontend frontend);

    /**
     * Create a new calculator from the given format file.
     */
    TuxCalculator.Builder createBy(TuxFrontend frontend, Path fmt);

    /**
     * Create a new calculator from the given format bytes.
     */
    TuxCalculator.Builder createBy(TuxFrontend frontend, byte[] fmt);

    /**
     * Create a new calculator from the given format stream.
     * This will close the stream.
     */
    TuxCalculator.Builder createBy(TuxFrontend frontend, InputStream fmt);

    static TuxCalculatorAPI get() {
        try {
            return (TuxCalculatorAPI) Class.forName("tuxcalculator.core.CalculatorAPI$").getField("MODULE$").get(null);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException("Could not get calculator API", e);
        }
    }
}
