package tuxcalculator.desktop;

import tuxcalculator.api.TuxCalculator;

import javax.annotation.Nullable;
import javax.annotation.OverridingMethodsMustInvokeSuper;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.stream.Stream;

public abstract class GraphicalFrontend extends DesktopFrontend {

    @Nullable private Consumer<Callable<Void>> executor;
    @Nullable private TuxCalculator calc;
    @Nullable private CalculatorHistory history;
    
    private String lastInput = "";
    private int arrowIdx = -1;
    private String current = "";
    
    @Nullable
    private List<String> tabList = null;
    private int tabIdx = -1;
    private int lastTabCurserPos = -1;

    protected abstract String getCurrentText();
    protected String getCurrentTextToCurser() {
        String text = this.getCurrentText();
        return text.substring(0, Math.min(text.length(), this.getCursorPosition()));
    }
    protected String getCurrentTextFromCurser() {
        String text = this.getCurrentText();
        return text.substring(Math.min(text.length(), this.getCursorPosition()));
    }
    protected abstract boolean supportsHighlighting();
    protected abstract void setCurrentText(String text);
    protected abstract void applyInputHighlighting(List<TuxCalculator.HighlightPart> highlightedText);
    protected abstract void grabInputFocus();
    protected abstract boolean hasSelectedText();
    protected abstract int getCursorPosition();
    protected abstract void placeCursorAt(int cursorPosition);
    protected abstract void appendLine(String term, List<TuxCalculator.HighlightPart> highlightTerm, TuxCalculator.Result result);
    
    @Override
    @OverridingMethodsMustInvokeSuper
    public void run(TuxCalculator calc, CalculatorHistory history, Consumer<Callable<Void>> executor) throws IOException {
        this.calc = calc;
        this.history = history;
        this.executor = executor;
    }

    protected final void perform(Action action) {
        if (executor != null) executor.accept(() -> {
            switch (action) {
                case SUBMIT -> {
                    this.delTab();
                    this.calcTerm();
                }
                case INCR_HIST -> {
                    this.delTab();
                    this.incrHist();
                }
                case DECR_HIST -> {
                    this.delTab();
                    this.decrHist();
                }
                case TAB_FORWARD -> this.updateTab(false);
                case TAB_BACKWARD -> this.updateTab(true);
                case STOP_TAB -> this.delTab();
                case HIGHLIGHT_INPUT -> this.applyInputHighlighting(this.doHighlight(this.getCurrentText()));
            }
            return null;
        });
    }
    
    private int clamp(int value, int minI, int maxE) {
        if (value < minI) return minI;
        if (value >= maxE) return maxE - 1;
        return value;
    }
    
    private List<TuxCalculator.HighlightPart> doHighlight(String text) {
        if (this.supportsHighlighting() && this.calc != null) {
            return this.calc.highlight(text);
        } else {
            return List.of(new TuxCalculator.HighlightPart(TuxCalculator.HighlightType.PLAIN, text));
        }
    }
    
    private void incrHist() {
        CalculatorHistory history = this.history;
        if (history == null) return;
        
        if (this.arrowIdx < 0) {
            this.current = this.getCurrentText();
        }

        this.arrowIdx = this.clamp(this.arrowIdx + 1, -1, history.length());
        String newText = this.arrowIdx < 0 ? this.current : history.get(this.arrowIdx);
        this.setCurrentText(newText);
        this.placeCursorAt(newText.length());
    }

    private void decrHist() {
        CalculatorHistory history = this.history;
        if (history == null) return;
        
        if (this.arrowIdx < 0) {
            this.current = this.getCurrentText();
        }
        
        this.arrowIdx = this.clamp(this.arrowIdx - 1, -1, history.length());
        String newText = this.arrowIdx < 0 ? this.current : history.get(this.arrowIdx);
        this.setCurrentText(newText);
        this.placeCursorAt(newText.length());
    }

    private void calcTerm() {
        if (this.calc == null) throw new IllegalStateException("Frontend has not been started yet.");
        
        String term = this.getCurrentText();
        this.setCurrentText("");
        this.placeCursorAt(0);

        if (term.isBlank()) term = this.lastInput;
        else this.lastInput = term;
        if (term.isBlank()) return;

        if (this.history != null) {
            this.history.add(term);
        }
        this.arrowIdx = -1;

        List<TuxCalculator.HighlightPart> highlightedTerm = this.doHighlight(term.strip());
        TuxCalculator.Result result = this.calc.parse(term);
        this.appendLine(term.strip(), highlightedTerm, result);
        this.grabInputFocus();
    }

    private void updateTab(boolean inverted) {
        if (this.calc == null) throw new IllegalStateException("Frontend has not been started yet.");
        if (this.hasSelectedText() || (this.lastTabCurserPos >= 0 && this.lastTabCurserPos != this.getCursorPosition())) {
            this.delTab();
            return;
        }
        
        String former;
        if (this.tabIdx < 0 || this.tabList == null) {
            TuxCalculator.TabCompletion completion = calc.tabComplete(this.getCurrentTextToCurser());
            
            former = completion.completionString();
            
            if (former.isEmpty() && completion.isIdentifier()) {
                this.tabList = Stream.concat(Stream.of("()"), completion.matches().stream()).toList();
            } else {
                this.tabList = completion.matches();
            }
            
            if (this.tabList.isEmpty()) {
                this.delTab();
                return;
            }
            
            if (inverted) {
                this.tabIdx = this.tabList.size() - 1;
            } else {
                this.tabIdx = 0;
            }
        } else {
            former = this.tabList.get(this.tabIdx);
            if (former.endsWith(")")) this.placeCursorAt(this.getCursorPosition() + 1);
            if (inverted) {
                this.tabIdx = (this.tabIdx + this.tabList.size() - 1) % this.tabList.size();
            } else {
                this.tabIdx = (this.tabIdx + 1) % this.tabList.size();
            }
        }
        
        String textBefore = this.getCurrentTextToCurser();
        String textAfter = this.getCurrentTextFromCurser();
        this.setCurrentText(textBefore.substring(0, textBefore.length() - former.length()) + this.tabList.get(this.tabIdx) + textAfter);
        if (this.tabList.get(this.tabIdx).endsWith(")")) {
            this.placeCursorAt(textBefore.length() - former.length() + this.tabList.get(this.tabIdx).length() - 1);
        } else {
            this.placeCursorAt(textBefore.length() - former.length() + this.tabList.get(this.tabIdx).length());
        }
        this.lastTabCurserPos = this.getCursorPosition();
    }

    private void delTab() {
        this.tabIdx = -1;
        this.tabList = null;
        this.lastTabCurserPos = -1;
    }
    
    protected enum Action {
        SUBMIT,
        INCR_HIST,
        DECR_HIST,
        TAB_FORWARD,
        TAB_BACKWARD,
        STOP_TAB,
        HIGHLIGHT_INPUT
    }
}
