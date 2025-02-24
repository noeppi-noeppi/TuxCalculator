package tuxcalculator.desktop;

import org.jline.keymap.KeyMap;
import org.jline.reader.*;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import org.jline.utils.InfoCmp;
import tuxcalculator.api.TuxCalculator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class TextFrontEnd extends DesktopFrontend {

    @Override
    public void showError(String err) {
        System.err.println(err);
    }

    @Override
    public void run(TuxCalculator calc, CalculatorHistory history, Consumer<Callable<Void>> executor) throws IOException {
        if (System.console() != null) {
            runJLine(calc, history);
        } else {
            runText(calc);
        }
    }
    
    private void runText(TuxCalculator calc) throws IOException {
        System.out.println(Main.title());
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            String line = reader.readLine();
            if (line == null) return;
            TuxCalculator.Result result = calc.parse(line);
            if (!(result instanceof TuxCalculator.Void)) System.out.println(result);
        }
    }
    
    private void runJLine(TuxCalculator calc, CalculatorHistory history) throws IOException {
        Logger.getLogger("org.jline").setLevel(Level.SEVERE);
        TerminalBuilder builder = TerminalBuilder.builder();
        builder.name("TuxCalculator");
        builder.system(true);
        builder.ffm(false);
        builder.jni(true);
        builder.jna(false);
        builder.jansi(false);
        try (Terminal terminal = builder.build()) {
            boolean isXTerm = terminal.getType() != null && terminal.getType().toLowerCase(Locale.ROOT).startsWith("xterm");
            terminal.echo(false);
            CalculatorHighlighter highlighter = new CalculatorHighlighter(calc);
            CalculatorCompleter completer = new CalculatorCompleter(calc);
            LineReader reader = LineReaderBuilder.builder()
                    .terminal(terminal)
                    .parser(new NothingParser())
                    .history(new HistoryWrapper(history))
                    .highlighter(highlighter)
                    .completer(completer)
                    .completionMatcher(completer)
                    .option(LineReader.Option.COMPLETE_MATCHER_TYPO, false)
                    .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
                    .option(LineReader.Option.AUTO_LIST, false)
                    .option(LineReader.Option.AUTO_MENU_LIST, false)
                    .option(LineReader.Option.RECOGNIZE_EXACT, false)
                    .option(LineReader.Option.ERASE_LINE_ON_FINISH, true)
                    .option(LineReader.Option.BRACKETED_PASTE, true)
                    .build();
            AtomicReference<TuxCalculator.Error> pendingError = new AtomicReference<>();
            reader.getWidgets().put("tuxc-show-trace", () -> {
                TuxCalculator.Error err = pendingError.getAndSet(null);
                if (err != null && !err.trace().isEmpty()) {
                    reader.callWidget(LineReader.CLEAR);
                    terminal.puts(InfoCmp.Capability.carriage_return);
                    for (String line : err.trace()) {
                        terminal.writer().println(ansiString("  " + line, AttributedStyle.BRIGHT + AttributedStyle.RED));
                    }
                    reader.callWidget(LineReader.REDRAW_LINE);
                    reader.callWidget(LineReader.REDISPLAY);
                    terminal.flush();
                    return true;
                } else {
                    return err != null;
                }
            });
            reader.getKeyMaps().get(LineReader.MAIN).bind(new Reference("tuxc-show-trace"), KeyMap.ctrl('t'));
            terminal.writer().println(Main.title());
            terminal.writer().flush();
            String lastInput = "";
            //noinspection InfiniteLoopStatement
            while (true) {
                if (isXTerm) {
                    terminal.writer().write("\u001B]0;" + Main.windowTitle() + "\u0007");
                    terminal.writer().flush();
                }
                String line = reader.readLine();
                if (line == null) continue;
                if (line.isBlank()) {
                    line = lastInput;
                } else {
                    lastInput = line;
                }
                if (line.isBlank()) continue;
                reader.printAbove(highlighter.highlight(reader, line));
                terminal.writer().flush();
                
                TuxCalculator.Result result = calc.parse(line);
                if (result instanceof TuxCalculator.Error err) {
                    terminal.writer().println(ansiString(result.toString(), AttributedStyle.BRIGHT + AttributedStyle.RED));
                    terminal.flush();
                    pendingError.set(err);
                } else if (!(result instanceof TuxCalculator.Void)) {
                    terminal.writer().println(ansiString(result.toString(), AttributedStyle.BRIGHT + AttributedStyle.CYAN));
                    terminal.flush();
                    pendingError.set(null);
                }
            }
        } catch (EndOfFileException | UserInterruptException e) {
            //
        }
    }
    
    private static String ansiString(String text, int color) {
        AttributedStringBuilder sb = new AttributedStringBuilder();
        sb.style(AttributedStyle.DEFAULT.foreground(color));
        sb.append(text);
        sb.style(AttributedStyle.DEFAULT);
        return sb.toAnsi();
    }

    private static final class HistoryWrapper extends DefaultHistory {
        
        private final CalculatorHistory history;
        
        public HistoryWrapper(CalculatorHistory history) {
            for (int i = history.length() - 1; i >= 0; i--) {
                this.add(history.get(i));
            }
            this.history = history;
        }

        @Override
        protected void internalAdd(Instant time, String line, boolean checkDuplicates) {
            if (this.history != null && this.first() == 0) this.history.add(line);
            super.internalAdd(time, line, checkDuplicates);
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
    
    @SuppressWarnings("ClassCanBeRecord")
    private static final class CalculatorHighlighter implements Highlighter {

        private final TuxCalculator calculator;

        private CalculatorHighlighter(TuxCalculator calculator) {
            this.calculator = calculator;
        }

        @Override
        public AttributedString highlight(LineReader reader, String buffer) {
            List<TuxCalculator.HighlightPart> parts = calculator.highlight(buffer);
            AttributedStringBuilder sb = new AttributedStringBuilder();
            for (TuxCalculator.HighlightPart part : parts) {
                AttributedStyle style = switch (part.type()) {
                    case NUMBER -> AttributedStyle.DEFAULT.foreground(159);
                    case GLOBAL -> AttributedStyle.DEFAULT.italic();
                    case OPERATOR -> AttributedStyle.DEFAULT.foreground(46);
                    case REFERENCE -> AttributedStyle.DEFAULT.foreground(184);
                    case SPECIAL -> AttributedStyle.DEFAULT.foreground(201);
                    case ERROR -> AttributedStyle.DEFAULT.foreground(203);
                    case COMMAND -> AttributedStyle.DEFAULT.bold().foreground(214);
                    case CONSTRUCT -> AttributedStyle.DEFAULT.foreground(219);
                    case COMMENT -> AttributedStyle.DEFAULT.foreground(246);
                    default -> AttributedStyle.DEFAULT;
                };
                sb.styled(style, part.content());
            }
            return sb.toAttributedString();
        }

        @Override
        public void setErrorPattern(Pattern errorPattern) {
            //
        }

        @Override
        public void setErrorIndex(int errorIndex) {
            //
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
