package tuxcalculator.desktop;

import org.apache.commons.text.StringEscapeUtils;
import tuxcalculator.api.TuxCalculator;
import tuxcalculator.api.TuxCalculatorAPI;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

public class SwingFrontend extends GraphicalFrontend {

    private JTextPane output;
    private String outputText = "";
    private JScrollPane outputScroll;
    private JTextField input;
    
    @Override
    public void showError(String err) {
        JOptionPane.showMessageDialog(null, err, "TuxCalculator - Error", JOptionPane.ERROR_MESSAGE);
    }

    @Override
    public void run(TuxCalculator calc, Consumer<Callable<Void>> executor) throws IOException {
        super.run(calc, executor);

        this.output = new JTextPane();
        this.output.setEditable(false);
        this.output.setContentType("text/html");
        this.output.setText("");
        this.outputScroll = new JScrollPane(this.output);
    
        this.input = new JTextField();
        this.input.setFocusTraversalKeysEnabled(false);
        
        JButton inputButton = new JButton("Apply");

        JFrame frame = new JFrame();
        frame.setTitle("TuxCalculator " + TuxCalculatorAPI.VERSION + (calc.ini() ? " (INI)" : ""));
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
                    SwingFrontend.this.perform(Action.SUBMIT);
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
        String inputPart = "<div>" + StringEscapeUtils.escapeHtml3(term) + "</div>";
        String outputPart;
        if (result instanceof TuxCalculator.Error) {
            outputPart = "<div align=\"right\"><font color=\"#CC0000\"><b>" + StringEscapeUtils.escapeHtml3(result.toString()) + "</b></font></div>";
        } else {
            outputPart = "<div align=\"right\"><b>" + StringEscapeUtils.escapeHtml3(result.toString()) + "</b></div>";
        }
        this.outputText = this.outputText + inputPart + outputPart;
        this.output.setText(this.outputText);
        JScrollBar scrollBar = this.outputScroll.getVerticalScrollBar();
        if (scrollBar != null) scrollBar.setValue(scrollBar.getMaximum());
    }
}
