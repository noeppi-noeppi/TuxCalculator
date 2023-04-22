package tuxcalculator.android.data;

@SuppressWarnings("ClassCanBeRecord")
public class TextEntry {
    
    public final String text;
    public final boolean result;
    public final boolean focusable;
    public final boolean red;

    public TextEntry(String text, boolean result, boolean focusable, boolean red) {
        this.text = text;
        this.result = result;
        this.focusable = focusable;
        this.red = red;
    }
}
