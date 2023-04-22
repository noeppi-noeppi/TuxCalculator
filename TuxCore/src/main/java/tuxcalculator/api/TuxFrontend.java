package tuxcalculator.api;

import java.io.IOException;
import java.io.OutputStream;

/**
 * An interface that encapsulates the necessary logic for TuxCalculator to interface with the outside world.
 */
public interface TuxFrontend {

    /**
     * Displays an error message in some way.
     */
    void showError(String err);

    /**
     * Opens a file given by file name for writing.
     * If the file does not exist, it should be created.
     * Otherwise, it should be truncated.
     */
    OutputStream openFile(String fileName) throws IOException;

    /**
     * Exits the running program.
     */
    void exit();
}
