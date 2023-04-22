package tuxcalculator.api;

import java.io.InputStream;
import java.io.Reader;
import java.nio.file.Path;
import java.util.List;

public interface TuxCalculator {

    /**
     * Gets whether the calculator is in ini-mode.
     */
    boolean ini();

    /**
     * Gets tab completion information for an input string.
     */
    TabCompletion tabComplete(String content);

    /**
     * Parses an input line.
     */
    Result parse(String line);

    sealed interface Result permits Success, Void, Error {}

    record Success(String value) implements Result {

        @Override
        public String toString() {
            return value;
        }
    }

    record Void() implements Result {

        @Override
        public String toString() {
            return "()";
        }
    }

    record Error(String message, List<String> trace) implements Result {

        @Override
        public String toString() {
            return "Error: " + message;
        }
    }

    /**
     * Tab completion result.
     *
     * @param prefix           The text before the currently tab-completed part starts.
     * @param completionString The string currently used for completion.
     * @param matches          A list of matches that in some way start with {@link #completionString}
     *                         (case insensitive, and according to unicode normalisation and removal of marks)
     * @param isIdentifier     Whether the tab completion covers a regular identifier ({@literal true} for an empty {@link #prefix})
     */
    record TabCompletion(String prefix, String completionString, List<String> matches, boolean isIdentifier) {}

    interface Builder {

        /**
         * Loads an additional file into the calculator.
         */
        void load(Path path);

        /**
         * Loads an additional file into the calculator.
         */
        void load(String fileName, InputStream in);

        /**
         * Loads an additional file into the calculator.
         */
        void load(String fileName, Reader in);

        /**
         * Checks for errors that have accumulated while building the calculator.
         * {@literal null} if there were none.
         */
        List<String> checkError();

        /**
         * Builds the calculator. Throws an exception if there were errors.
         */
        TuxCalculator build();
    }
}
