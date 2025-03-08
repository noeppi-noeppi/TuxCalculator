package tuxcalculator.android;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Typeface;
import android.text.Editable;
import android.text.Spannable;
import android.text.Spanned;
import android.text.TextWatcher;
import android.text.style.CharacterStyle;
import android.text.style.ForegroundColorSpan;
import android.text.style.StyleSpan;
import tuxcalculator.android.data.Highlight;
import tuxcalculator.api.TuxCalculator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class HighlightHelper {

    public static List<Highlight> computeHighlights(TuxCalculator calculator, String text) {
        List<TuxCalculator.HighlightPart> parts = calculator.highlight(text);
        List<Highlight> highlights = new ArrayList<>(parts.size());
        int offset = 0;
        for (TuxCalculator.HighlightPart part : parts) {
            if (part.type() != TuxCalculator.HighlightType.PLAIN) {
                highlights.add(new Highlight(part.type(), offset, offset + part.content().length()));
            }
            offset += part.content().length();
        }
        return Collections.unmodifiableList(highlights);
    }
    
    public static void unhighlight(Spannable spannable) {
        CharacterStyle[] colorSpans = spannable.getSpans(0, spannable.length(), ForegroundColorSpan.class);
        for (CharacterStyle span : colorSpans) spannable.removeSpan(span);
        CharacterStyle[] styleSpans = spannable.getSpans(0, spannable.length(), StyleSpan.class);
        for (CharacterStyle span : styleSpans) spannable.removeSpan(span);
    }

    public static void highlight(Context context, Spannable spannable, int offset, List<Highlight> highlights) {
        for (Highlight highlight : highlights) {
            List<CharacterStyle> styles = styles(context, highlight.type);
            for (CharacterStyle style : styles) {
                int start = clamp(spannable, offset + highlight.start);
                int end = clamp(spannable, offset + highlight.end);
                spannable.setSpan(style, start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
            }
        }
    }
    
    private static int clamp(Spannable spannable, int idx) {
        if (idx < 0) idx = 0;
        return Math.min(idx, spannable.length());
    }
    
    private static List<CharacterStyle> styles(Context context, TuxCalculator.HighlightType type) {
        boolean light = (context.getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_NO;
        CharacterStyle lightBold = light ? new StyleSpan(Typeface.BOLD) : new StyleSpan(Typeface.NORMAL);
        return switch (type) {
            case NUMBER -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_number, context.getTheme())));
            case GLOBAL -> List.of(new StyleSpan(Typeface.ITALIC));
            case OPERATOR -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_operator, context.getTheme())), lightBold);
            case REFERENCE -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_reference, context.getTheme())), lightBold);
            case SPECIAL -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_special, context.getTheme())));
            case ERROR -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_error, context.getTheme())));
            case COMMAND -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_command, context.getTheme())), new StyleSpan(Typeface.BOLD));
            case CONSTRUCT -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_construct, context.getTheme())));
            case COMMENT -> List.of(new ForegroundColorSpan(context.getResources().getColor(R.color.highlight_comment, context.getTheme())));
            default -> List.of();
        };
    }

    public static TextWatcher createHighlighter(Context context, TuxCalculator calculator) {
        return new TextWatcher() {
            
            @Override
            public void beforeTextChanged(CharSequence text, int start, int count, int after) {
                //
            }

            @Override
            public void onTextChanged(CharSequence text, int start, int before, int count) {
                //
            }

            @Override
            public void afterTextChanged(Editable editable) {
                List<Highlight> highlights = computeHighlights(calculator, editable.toString());
                unhighlight(editable);
                highlight(context, editable, 0, highlights);
            }
        };
    }
}
