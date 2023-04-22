package tuxcalculator.desktop;

import org.fusesource.jansi.Ansi;
import org.jline.reader.*;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import tuxcalculator.api.TuxCalculator;
import tuxcalculator.api.TuxCalculatorAPI;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class TextFrontEnd extends DesktopFrontend {

    @Override
    public void showError(String err) {
        System.err.println(err);
    }

    @Override
    public void run(TuxCalculator calc, Consumer<Callable<Void>> executor) throws IOException {
        if (System.console() != null) {
            runJLine(calc);
        } else {
            runText(calc);
        }
    }
    
    private void runText(TuxCalculator calc) throws IOException {
        System.out.println("This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION + (calc.ini() ? " (INI)" : ""));
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        while(true) {
            String line = reader.readLine();
            if (line == null) return;
            TuxCalculator.Result result = calc.parse(line);
            if (!(result instanceof TuxCalculator.Void)) System.out.println(result);
        }
    }
    
    private void runJLine(TuxCalculator calc) throws IOException {
        Logger.getLogger("org.jline").setLevel(Level.SEVERE);
        TerminalBuilder builder = TerminalBuilder.builder();
        builder.name("TuxCalculator");
        builder.system(true);
        builder.jansi(true);
        builder.jna(false);
        try (Terminal terminal = builder.build()) {
            CalculatorCompleter completer = new CalculatorCompleter(calc);
            LineReader reader = LineReaderBuilder.builder()
                    .terminal(terminal)
                    .parser(new NothingParser())
                    .history(new DefaultHistory())
                    .completer(completer)
                    .completionMatcher(completer)
                    .option(LineReader.Option.COMPLETE_MATCHER_TYPO, false)
                    .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
                    .option(LineReader.Option.AUTO_LIST, false)
                    .option(LineReader.Option.AUTO_MENU_LIST, false)
                    .option(LineReader.Option.RECOGNIZE_EXACT, false)
                    .build();
            System.out.println("This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION + (calc.ini() ? " (INI)" : ""));
            //noinspection InfiniteLoopStatement
            while (true) {
                String line = reader.readLine(Ansi.ansi().reset().fgBrightGreen().toString());
                if (line == null) continue;
                TuxCalculator.Result result = calc.parse(line);
                if (result instanceof TuxCalculator.Error) {
                    terminal.writer().println(Ansi.ansi().reset().fgBrightRed().a(result).reset().toString());
                    terminal.flush();
                } else if (!(result instanceof TuxCalculator.Void)) {
                    terminal.writer().println(Ansi.ansi().reset().fgBrightCyan().a(result).reset().toString());
                    terminal.flush();
                }
            }
        } catch (EndOfFileException | UserInterruptException e) {
            //
        }
    }

    private static final class NothingParser implements Parser {

        @Override
        public ParsedLine parse(String line, int cursor, ParseContext context) throws SyntaxError {
            if (cursor < 0 || cursor > line.length()) throw new SyntaxError(1, cursor, "Invalid cursor");
            return new SimpleParsedLine(line.substring(0, cursor), line.substring(cursor), cursor);
        }

        @Override
        public boolean isEscapeChar(char ch) {
            return false;
        }

        @Override
        public boolean validCommandName(String name) {
            return false;
        }

        @Override
        public boolean validVariableName(String name) {
            return false;
        }
    }
    
    private record SimpleParsedLine(String preCursor, String postCursor, int cursor) implements ParsedLine {

        @Override
        public String line() {
            return preCursor() + postCursor();
        }

        @Override
        public String word() {
            return preCursor();
        }

        @Override
        public int wordCursor() {
            return cursor();
        }

        @Override
        public int wordIndex() {
            return 0;
        }

        @Override
        public List<String> words() {
            return List.of(preCursor(), postCursor());
        }
    }
    
    private static final class CalculatorCompleter implements Completer, CompletionMatcher {

        private final TuxCalculator calculator;
        private TuxCalculator.TabCompletion completion;

        private CalculatorCompleter(TuxCalculator calculator) {
            this.calculator = calculator;
        }
        
        private List<Candidate> getCandidates() {
            if (this.completion == null) return new ArrayList<>();
            return IntStream.range(0, this.completion.matches().size())
                    .mapToObj(idx -> new Candidate(
                            this.completion.prefix() + this.completion.matches().get(idx),
                            this.completion.matches().get(idx), 
                            null, null, null, null,
                            false, 1 + idx
                    ))
                    .collect(Collectors.toList());
        }
        
        @Override
        public void complete(LineReader reader, ParsedLine line, List<Candidate> candidates) {
            this.completion = calculator.tabComplete(line.word());
            candidates.addAll(this.getCandidates());
        }

        @Override
        public void compile(Map<LineReader.Option, Boolean> options, boolean prefix, CompletingParsedLine line, boolean caseInsensitive, int errors, String originalGroupName) {
            this.completion = calculator.tabComplete(line.word());
        }

        @Override
        public List<Candidate> matches(List<Candidate> candidates) {
            return this.getCandidates();
        }

        @Override
        public Candidate exactMatch() {
            return null;
        }

        @Override
        public String getCommonPrefix() {
            return this.completion.completionString();
        }
    }
}
