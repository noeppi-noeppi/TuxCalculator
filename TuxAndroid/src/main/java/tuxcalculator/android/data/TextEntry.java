package tuxcalculator.android.data;

import androidx.annotation.Nullable;

@SuppressWarnings("ClassCanBeRecord")
public class TextEntry {
    
    public final String text;
    public final boolean result;
    public final boolean focusable;
    public final boolean red;
    @Nullable public final String detail;

    public TextEntry(String text, boolean result, boolean focusable, boolean red, String detail) {
        this.text = text;
        this.result = result;
        this.focusable = focusable;
        this.red = red;
        this.detail = detail;
    }
}
