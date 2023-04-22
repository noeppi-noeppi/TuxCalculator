package tuxcalculator.android;

import android.text.Selection;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import tuxcalculator.api.TuxCalculator;

import java.util.List;
import java.util.stream.Collectors;

public class TabCompletionAdapter extends BaseAdapter implements Filterable {
    
    private final MainActivity activity;
    private final TuxCalculator calculator;
    private final AutoCompleteTextView view;
    private List<Suggestion> completion;

    public TabCompletionAdapter(MainActivity activity, TuxCalculator calculator, AutoCompleteTextView view) {
        this.activity = activity;
        this.calculator = calculator;
        this.view = view;
        this.completion = List.of();
    }
    
    @Override
    public int getCount() {
        return this.completion.size();
    }

    @Override
    public Object getItem(int position) {
        return this.completion.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        TextView view;
        if (convertView instanceof TextView tv) {
            view = tv;
        } else {
            View theView = this.activity.getLayoutInflater().inflate(R.layout.autocomplete, parent, false);
            if (!(theView instanceof TextView tv)) throw new IllegalStateException("autocomplete view is not a text view.");
            view = tv;
        }
        view.setText(this.completion.get(position).name);
        return view;
    }

    @Override
    public Filter getFilter() {
        return new TheFilter();
    }
    
    private class TheFilter extends Filter {

        @Override
        protected FilterResults performFiltering(CharSequence prefixSq) {
            if (prefixSq == null) prefixSq = "";
            
            TuxCalculator.TabCompletion completion = TabCompletionAdapter.this.calculator.tabComplete(prefixSq.toString());
            List<Suggestion> matches = completion.matches().stream()
                    .map(entry -> new Suggestion(entry, completion.prefix() + entry))
                    .collect(Collectors.toList());
            
            FilterResults results = new FilterResults();
            results.count = matches.size();
            results.values = matches;
            return results;
        }

        @Override
        protected void publishResults(CharSequence prefix, FilterResults results) {
            // Called on the main thread, check that cursor is at the end;
            // completion mid-text is not supported
            if (Selection.getSelectionStart(TabCompletionAdapter.this.view.getText()) != prefix.length() || Selection.getSelectionEnd(TabCompletionAdapter.this.view.getText()) != prefix.length()) {
                TabCompletionAdapter.this.completion = List.of();
                TabCompletionAdapter.this.notifyDataSetInvalidated();
            } else {
                //noinspection unchecked
                TabCompletionAdapter.this.completion = (List<Suggestion>) results.values;
                if (results.count > 0) TabCompletionAdapter.this.notifyDataSetChanged();
                else TabCompletionAdapter.this.notifyDataSetInvalidated();
            }
        }

        @Override
        public CharSequence convertResultToString(Object resultValue) {
            if (resultValue instanceof Suggestion suggestion) {
                return suggestion.fullString;
            } else {
                return resultValue.toString();
            }
        }
    }

    @SuppressWarnings("ClassCanBeRecord")
    static final class Suggestion {

        public final String name;
        public final String fullString;

        Suggestion(String name, String fullString) {
            this.name = name;
            this.fullString = fullString;
        }
    }
}
