package tuxcalculator.android.data;

import androidx.annotation.Nullable;

import java.util.List;

@SuppressWarnings("ClassCanBeRecord")
public class TextEntry {
    
    public final String text;
    public final boolean result;
    public final boolean focusable;
    public final boolean red;
    @Nullable public final String detail;
    public final List<Highlight> highlight;

    public TextEntry(String text, boolean result, boolean focusable, boolean red, @Nullable String detail, List<Highlight> highlight) {
        this.text = text;
        this.result = result;
        this.focusable = focusable;
        this.red = red;
        this.detail = detail;
        this.highlight = highlight;
    }
}
