package tuxcalculator.android.data;

import tuxcalculator.api.TuxCalculator;

@SuppressWarnings("ClassCanBeRecord")
public class Highlight {

    public final TuxCalculator.HighlightType type;
    public final int start;
    public final int end;
    
    public Highlight(TuxCalculator.HighlightType type, int start, int end) {
        this.start = start;
        this.end = end;
        this.type = type;
    }
}
