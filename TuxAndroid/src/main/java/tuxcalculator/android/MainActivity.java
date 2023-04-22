package tuxcalculator.android;

import android.graphics.Typeface;
import android.os.Bundle;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import com.google.android.material.snackbar.Snackbar;
import tuxcalculator.android.data.TextEntry;
import tuxcalculator.android.dialog.SimpleDialogFragment;
import tuxcalculator.api.TuxCalculatorAPI;

public class MainActivity extends AppCompatActivity {

    private ApplicationState state;
    
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.setContentView(R.layout.activity_main);
        this.state = ApplicationState.get(this);
    }

    @Override
    protected void onPause() {
        state.updateTermInput(((EditText) this.findViewById(R.id.term_input)).getText().toString());
        if (this.isFinishing()) ApplicationState.finish();
        super.onPause();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        this.getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == R.id.menu_about) {
            SimpleDialogFragment.showDialog(this, this.getResources().getString(R.string.menu_about), "This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION + "\n\n" + this.getResources().getString(R.string.about));
            return true;
        } else {
            return super.onOptionsItemSelected(item);
        }
    }

    public void startLoad() {
        this.findViewById(R.id.load_spinner).setVisibility(View.VISIBLE);
        this.findViewById(R.id.term_input).setEnabled(false);
        this.findViewById(R.id.submit).setEnabled(false);
    }

    public void endLoad() {
        this.findViewById(R.id.load_spinner).setVisibility(View.GONE);
        this.findViewById(R.id.term_input).setEnabled(true);
        this.findViewById(R.id.submit).setEnabled(true);
    }
    
    public void showErrorPopup(String err) {
        Snackbar snackbar = Snackbar.make(this.requireViewById(R.id.root), err, Snackbar.LENGTH_INDEFINITE);
        snackbar.setTextMaxLines(20);
        snackbar.setAction(R.string.dismiss, v -> snackbar.dismiss());
        snackbar.show();
    }

    public void showErrorState(String text) {
        this.endLoad();
        
        View bar = this.findViewById(R.id.userbar);
        bar.setVisibility(View.GONE);

        LinearLayout layout = this.findViewById(R.id.text_output);
        layout.removeAllViews();

        this.addTextView(new TextEntry(text, false, false, false));
    }

    public void addTextView(TextEntry entry) {
        this.endLoad();
        
        TextView view = new TextView(this);
        view.setText(entry.text);
        view.setLayoutParams(new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT));
        view.setTextSize(TypedValue.COMPLEX_UNIT_PX, this.getResources().getDimensionPixelSize(R.dimen.text_size));
        if (entry.focusable) {
            view.setTextIsSelectable(true);
            view.setSelectAllOnFocus(true);
            view.setFocusedByDefault(false);
            view.setOnFocusChangeListener((v, hasFocus) -> {if (hasFocus) {
                InputMethodManager mgr = this.getSystemService(InputMethodManager.class);
                mgr.hideSoftInputFromWindow(v.getWindowToken(), 0);
            }});
        }
        if (entry.red) {
            view.setTextColor(this.getResources().getColor(R.color.error, this.getTheme()));
        }
        if (entry.result) {
            view.setTypeface(view.getTypeface(), Typeface.BOLD);
            view.setTextAlignment(View.TEXT_ALIGNMENT_TEXT_END);
        } else {
            view.setTextAlignment(View.TEXT_ALIGNMENT_TEXT_START);
        }

        LinearLayout layout = this.findViewById(R.id.text_output);
        ScrollView scroll = this.findViewById(R.id.text_scroll);

        layout.addView(view);
        this.getMainExecutor().execute(() -> scroll.scrollToDescendant(view));
    }
}
