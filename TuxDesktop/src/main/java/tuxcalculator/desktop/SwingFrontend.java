package tuxcalculator.desktop;

import org.apache.commons.text.StringEscapeUtils;
import tuxcalculator.api.TuxCalculator;

import javax.annotation.Nullable;
import javax.swing.*;
import java.awt.*;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

public class SwingFrontend extends GraphicalFrontend {

    private JTextPane output;
    private String outputText = "";
    private JScrollPane outputScroll;
    private JTextArea input;
    private Font font;
    
    @Override
    public void showError(String err) {
        JOptionPane.showMessageDialog(null, err, "TuxCalculator - Error", JOptionPane.ERROR_MESSAGE);
    }

    @Override
    public void run(TuxCalculator calc, CalculatorHistory history, Consumer<Callable<Void>> executor) throws IOException {
        super.run(calc, history, executor);
        
        this.font = selectFont();

        this.output = new JTextPane();
        this.output.setEditable(false);
        this.output.getCaret().setVisible(false);
        this.output.addCaretListener(e -> this.output.getCaret().setVisible(false));
        this.output.setContentType("text/html");
        this.output.setText("");
        if (this.font != null) this.output.setFont(this.font);
        this.outputScroll = new JScrollPane(this.output);
    
        this.input = new JTextArea();
        this.input.setFocusTraversalKeysEnabled(false);
        this.input.setLineWrap(true);
        if (this.font != null) this.input.setFont(this.font);
        
        JButton inputButton = new JButton("Apply");
        if (this.font != null) inputButton.setFont(this.font.deriveFont(Font.BOLD));
        Insets oldMargin = inputButton.getMargin();
        inputButton.setMargin(new Insets(oldMargin.top / 2, oldMargin.left / 2, oldMargin.bottom / 2, oldMargin.right / 2));

        JFrame frame = new JFrame();
        frame.setTitle(Main.windowTitle());
        GridBagLayout layout = new GridBagLayout();
        frame.setLayout(layout);
    
        frame.add(this.outputScroll);
        frame.add(this.input);
        frame.add(inputButton);

        this.outputScroll.setPreferredSize(new Dimension(440, 220));
        GridBagConstraints cOutput = new GridBagConstraints();
        cOutput.gridx = 0;
        cOutput.gridwidth = 2;
        cOutput.gridy = 0;
        cOutput.fill = GridBagConstraints.BOTH;
        cOutput.weightx = 0;
        cOutput.weighty = 1;
        layout.setConstraints(this.outputScroll, cOutput);

        GridBagConstraints cInput = new GridBagConstraints();
        cInput.gridx = 0;
        cInput.gridy = 1;
        cInput.fill = GridBagConstraints.HORIZONTAL;
        cInput.weightx = 1;
        cInput.weighty = 0;
        layout.setConstraints(this.input, cInput);

        GridBagConstraints cInputButton = new GridBagConstraints();
        cInputButton.gridx = 1;
        cInputButton.gridy = 1;
        cInputButton.fill = GridBagConstraints.NONE;
        cInputButton.weightx = 0;
        cInputButton.weighty = 0;
        layout.setConstraints(inputButton, cInputButton);

        this.input.addKeyListener(new KeyAdapter() {
            
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    if ((e.getModifiersEx() & InputEvent.ALT_DOWN_MASK) == 0) {
                        SwingFrontend.this.perform(Action.SUBMIT);
                    } else {
                        // Alt+Enter doesn't insert a line break in a JTextArea by default, so we do that manually.
                        SwingFrontend.this.input.replaceSelection("\n");
                    }
                    e.consume();
                } else if (e.getKeyCode() == KeyEvent.VK_UP || e.getKeyCode() == KeyEvent.VK_KP_UP) {
                    SwingFrontend.this.perform(Action.INCR_HIST);
                    e.consume();
                } else if (e.getKeyCode() == KeyEvent.VK_DOWN || e.getKeyCode() == KeyEvent.VK_KP_DOWN) {
                    SwingFrontend.this.perform(Action.DECR_HIST);
                    e.consume();
                } else if (e.getKeyCode() == KeyEvent.VK_TAB) {
                    SwingFrontend.this.perform(e.isShiftDown() ? Action.TAB_BACKWARD : Action.TAB_FORWARD);
                    e.consume();
                } else {
                    super.keyPressed(e);
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
                if (e.getKeyChar() != KeyEvent.CHAR_UNDEFINED && e.getKeyChar() != '\t') SwingFrontend.this.perform(Action.STOP_TAB);
                super.keyTyped(e);
            }
        });
        
        inputButton.addActionListener(e -> this.perform(Action.SUBMIT));
        
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setVisible(true);
        this.grabInputFocus();
    
        Object obj = new Object();
        //noinspection SynchronizationOnLocalVariableOrMethodParameter
        synchronized (obj) {
            try {
                obj.wait();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    protected String getCurrentText() {
        return this.input.getText();
    }

    @Override
    protected void setCurrentText(String text) {
        this.input.setText(text);
    }

    @Override
    protected void grabInputFocus() {
        this.input.grabFocus();
    }

    @Override
    protected boolean hasSelectedText() {
        return this.input.getSelectedText() != null;
    }

    @Override
    protected int getCursorPosition() {
        return this.input.getCaret().getDot();
    }

    @Override
    protected void placeCursorAt(int cursorPosition) {
        this.input.getCaret().setDot(cursorPosition);
    }

    @Override
    protected void appendLine(String term, TuxCalculator.Result result) {
        String inputPart = "<div align=\"left\">" + this.makeFontTag(term, null) + "</div>";
        String outputPart;
        if (result instanceof TuxCalculator.Error) {
            outputPart = "<div align=\"right\"><b>" + this.makeFontTag(result.toString(), "#CC0000") + "</b></div>";
        } else {
            outputPart = "<div align=\"right\"><b>" + this.makeFontTag(result.toString(), null) + "</b></div>";
        }
        this.outputText = this.outputText + inputPart + outputPart;
        this.output.setText(this.outputText);
        JScrollBar scrollBar = this.outputScroll.getVerticalScrollBar();
        if (scrollBar != null) scrollBar.setValue(scrollBar.getMaximum());
    }
    
    private String makeFontTag(String text, @Nullable String color) {
        String escapedText = StringEscapeUtils.escapeHtml3(text).replace("\r", "").replace("\n", "<br>");
        if (this.font == null && color == null) {
            return escapedText;
        } else {
            String fontPart = this.font == null ? "" : " face=\"" + StringEscapeUtils.escapeJson(this.font.getFamily()) + "\"";
            String sizePart = this.font == null ? "" : " style=\"font-size:" + StringEscapeUtils.escapeJson(this.font.getSize2D() + "pt") + ";\"";
            String colorPart = color == null ? "" : " color=\"" + StringEscapeUtils.escapeJson(color) + "\"";
            return "<font" + fontPart + sizePart + colorPart + ">" + escapedText + "</font>";
        }
    }

    @Nullable
    private static Font selectFont() {
        Font family = selectFontFamily();
        if (family == null) return null;
        Font defaultFont = new JLabel().getFont();
        return family.deriveFont(defaultFont.getSize2D() + 1.4f);
    }

    @Nullable
    private static Font selectFontFamily() {
        Font notoSans = null;
        Font openSans = null;
        Font roboto = null;
        Font arial = null;
        Font helvetica = null;
        for (Font font : GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts()) {
            if (font.getStyle() != Font.PLAIN) continue;
            if ("noto sans".equalsIgnoreCase(font.getFamily())) notoSans = font;
            if ("open sans".equalsIgnoreCase(font.getFamily())) openSans = font;
            if ("roboto".equalsIgnoreCase(font.getFamily())) roboto = font;
            if ("arial".equalsIgnoreCase(font.getFamily())) arial = font;
            if ("helvetica".equalsIgnoreCase(font.getFamily())) helvetica = font;
        }
        if (notoSans != null) return notoSans;
        if (openSans != null) return openSans;
        if (roboto != null) return roboto;
        if (arial != null) return arial;
        if (helvetica != null) return helvetica;
        return null;
    }
}
