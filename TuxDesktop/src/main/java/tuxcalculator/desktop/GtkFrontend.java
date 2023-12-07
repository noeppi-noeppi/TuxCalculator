package tuxcalculator.desktop;

import org.gnome.gdk.Keyval;
import org.gnome.gdk.Rectangle;
import org.gnome.gtk.*;
import org.gnome.pango.Weight;
import tuxcalculator.api.TuxCalculator;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

public class GtkFrontend extends GraphicalFrontend {

    private TextView in;
    private TextView out;
    private ScrolledWindow scrollPane;

    private TextTag outTag;
    private TextTag outTagError;

    private final List<TextTagKey> outputTags = new ArrayList<>();
    
    @Override
    public void init() {
        if (!Gtk.isInitialized()) Gtk.init(new String[]{});
    }

    @Override
    public void showError(String err) {
        TextBuffer buffer = new TextBuffer();
        buffer.setText(err);
        
        Dialog dialog = new Dialog();
        dialog.setTitle("TuxCalculator - Error");
        dialog.add(new Label(err));
        dialog.addButton(Stock.CLOSE, ResponseType.CLOSE);
    
        dialog.showAll();
        dialog.run();
    }

    @Override
    public void run(TuxCalculator calc, Consumer<Callable<Void>> executor) throws IOException {
        super.run(calc, executor);
        
        try (InputStream in = GtkFrontend.class.getResourceAsStream("/tuxcalculator/desktop/gtk_window.xml")) {
            if (in == null) throw new IOException("GTK window definition not found");
            String windowDef = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(in.readAllBytes())).toString();
            
            Builder builder = new Builder();
            builder.addFromString(windowDef);
            
            Window window = (Window) builder.getObject("tux_window");
            window.setTitle(Main.windowTitle());
            window.connect((Widget.Hide) widget -> this.exit());
            
            this.in = (TextView) builder.getObject("tux_term_input");
            this.out = (TextView) builder.getObject("tux_term_output");
            Button enter = (Button) builder.getObject("tux_term_enter");
            this.scrollPane = (ScrolledWindow) builder.getObject("tux_term_scroll");

            TextTagTable inTable = new TextTagTable();
            this.in.setBuffer(new TextBuffer(inTable));

            TextTagTable outTable = new TextTagTable();
            this.out.setBuffer(new TextBuffer(outTable));

            this.outTag = new TextTag(outTable);
            this.outTag.setJustify(Justification.RIGHT);
            this.outTag.setWeight(Weight.BOLD);
            
            this.outTagError = new TextTag(outTable);
            this.outTagError.setJustify(Justification.RIGHT);
            this.outTagError.setWeight(Weight.BOLD);
            this.outTagError.setForeground("#CC0000");

            this.in.connect((Widget.KeyPressEvent) (widget, eventKey) -> {
                if (eventKey.getKeyval() == Keyval.Return || eventKey.getKeyval().toString().toLowerCase(Locale.ROOT).contains("kp_enter")) {
                    this.perform(Action.SUBMIT);
                    return true;
                } else if (eventKey.getKeyval() == Keyval.Up || eventKey.getKeyval().toString().toLowerCase(Locale.ROOT).contains("kp_up")) {
                    this.perform(Action.INCR_HIST);
                    return true;
                } else if (eventKey.getKeyval() == Keyval.Down || eventKey.getKeyval().toString().toLowerCase(Locale.ROOT).contains("kp_down")) {
                    this.perform(Action.DECR_HIST);
                    return true;
                } else if (eventKey.getKeyval() == Keyval.Tab) {
                    this.perform(Action.TAB_FORWARD);
                    return true;
                } else if (eventKey.getKeyval() == Keyval.BackTab) {
                    this.perform(Action.TAB_BACKWARD);
                    return true;
                } else {
                    if (eventKey.getKeyval().toUnicode() != 0) this.perform(Action.STOP_TAB);
                    return false;
                }
            });

            enter.connect((Button.Clicked) button -> this.perform(Action.SUBMIT));
            this.out.connect((Widget.SizeAllocate) (widget, rectangle) -> this.scrollPane.getVAdjustment().setValue(this.scrollPane.getVAdjustment().getUpper()));
            this.out.connect((Widget.MotionNotifyEvent) (source, event) -> {
                int x = this.out.convertWindowToBufferCoordsX(TextWindowType.TEXT, (int) event.getX());
                int y = this.out.convertWindowToBufferCoordsY(TextWindowType.TEXT, (int) event.getY());
                TextIter iter = this.out.getIterAtLocation(x, y);
                
                // Check that the cursor is actually roughly in the iters bounds.
                // If no text is hovered, we'll just get the closest iter but don't want to show a tooltip then.
                Rectangle bounds = this.out.getLocation(iter);
                if (bounds.getX() - 3 > x || bounds.getX() + bounds.getWidth() + 3 < x || bounds.getY() - 3 > y || bounds.getY() + bounds.getHeight() + 3 < y) {
                    this.out.setTooltipText("");
                    return false;
                }
                
                int cursor = iter.getOffset();
                String tooltip = null;
                for (TextTagKey key : this.outputTags) {
                    if (key.start < cursor && key.end > cursor) {
                        tooltip = key.tooltip();
                    }
                }
                this.out.setTooltipText(tooltip == null ? "" : tooltip);
                return false;
            });
            
            this.grabInputFocus();
            window.showAll();
            window.present();
            Gtk.main();
        } catch (ParseException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void exit() {
        Gtk.mainQuit();
        super.exit();
    }

    @Override
    protected String getCurrentText() {
        return this.in.getBuffer().getText();
    }

    @Override
    protected void setCurrentText(String text) {
        this.in.getBuffer().setText(text);
    }

    @Override
    protected void grabInputFocus() {
        this.in.grabFocus();
    }

    @Override
    protected boolean hasSelectedText() {
        return this.in.getBuffer().getHasSelection();
    }

    @Override
    protected int getCursorPosition() {
        return this.in.getBuffer().getCursorPosition();
    }

    @Override
    protected String getCurrentTextToCurser() {
        return this.in.getBuffer().getText(this.in.getBuffer().getIterStart(), this.in.getBuffer().getIter(this.in.getBuffer().getInsert()), true);
    }

    @Override
    protected String getCurrentTextFromCurser() {
        return this.in.getBuffer().getText(this.in.getBuffer().getIter(this.in.getBuffer().getInsert()), this.in.getBuffer().getIterEnd(), true);
    }

    @Override
    protected void placeCursorAt(int cursorPosition) {
        this.in.getBuffer().placeCursor(this.in.getBuffer().getIter(cursorPosition));
    }

    @Override
    protected void appendLine(String term, TuxCalculator.Result result) {
        if (this.out.getBuffer().getText().isEmpty()) {
            this.out.getBuffer().setText(term);
        } else {
            this.out.getBuffer().setText(this.out.getBuffer().getText() + "\n" + term);
        }
        
        int start = this.out.getBuffer().getCharCount();
        this.out.getBuffer().setText(this.out.getBuffer().getText() + "\n" + result);
        int end = this.out.getBuffer().getCharCount();
        
        if (result instanceof TuxCalculator.Error err) {
            String tooltip = null;
            if (!err.trace().isEmpty()) tooltip = String.join("\n", err.trace());
            this.outputTags.add(new TextTagKey(start, end, tooltip, this.outTagError));
        } else {
            this.outputTags.add(new TextTagKey(start, end, null, this.outTag));
        }
        
        this.out.getBuffer().removeAllTags(this.out.getBuffer().getIterStart(), this.out.getBuffer().getIterEnd());
        for (TextTagKey tag : this.outputTags) {
            this.out.getBuffer().applyTag(tag.tag(), this.out.getBuffer().getIter(tag.start()), this.out.getBuffer().getIter(tag.end()));
        }

        this.scrollPane.getVAdjustment().setValue(this.scrollPane.getVAdjustment().getUpper());
    }

    private record TextTagKey(int start, int end, @Nullable String tooltip, TextTag tag) {}
}
