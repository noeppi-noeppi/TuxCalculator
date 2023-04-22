package tuxcalculator.android.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.method.LinkMovementMethod;
import android.text.util.Linkify;
import android.util.TypedValue;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.fragment.app.DialogFragment;
import androidx.fragment.app.FragmentActivity;
import tuxcalculator.android.R;

import java.util.concurrent.atomic.AtomicReference;

public class SimpleDialogFragment extends DialogFragment {
    
    @Nullable private String title;
    @Nullable private String message;

    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {
        this.title = this.getValue(savedInstanceState, "TCD_title");
        this.message = this.getValue(savedInstanceState, "TCD_message");
        if (this.message == null) this.message = "";
        
        TextView messageView = new TextView(this.requireContext());
        SpannableString msg = new SpannableString(this.message);
        Linkify.addLinks(msg, Linkify.WEB_URLS);
        messageView.setText(msg);
        messageView.setMovementMethod(LinkMovementMethod.getInstance());
        int paddingPx = this.getResources().getDimensionPixelSize(R.dimen.padding);
        messageView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
        messageView.setTextSize(TypedValue.COMPLEX_UNIT_PX, this.getResources().getDimensionPixelSize(R.dimen.text_size));

        AtomicReference<AlertDialog> dialogRef = new AtomicReference<>();
        AlertDialog.Builder builder = new AlertDialog.Builder(this.requireContext());
        if (this.title != null) builder.setTitle(this.title);
        builder.setView(messageView);
        builder.setNeutralButton(this.getString(R.string.close), (dialog1, which) -> dialogRef.get().dismiss());
        AlertDialog dialog = builder.create();
        dialogRef.set(dialog);
        return dialog;
    }
    
    @Nullable
    private String getValue(@Nullable Bundle savedInstanceState, String key) {
        if (savedInstanceState != null && savedInstanceState.containsKey(key)) {
            return savedInstanceState.getString(key);
        } else if (this.getArguments() != null && this.getArguments().containsKey(key)) {
            return this.getArguments().getString(key);
        } else {
            return null;
        }
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        if (this.title != null) outState.putString("TCD_title", this.title);
        if (this.message != null) outState.putString("TCD_message", this.message);
    }
    
    public static void showDialog(FragmentActivity activity, @Nullable String title, String message) {
        Bundle args = new Bundle();
        if (title != null) args.putString("TCD_title", title);
        if (message != null) args.putString("TCD_message", message);
        SimpleDialogFragment fragment = new SimpleDialogFragment();
        fragment.setArguments(args);
        fragment.show(activity.getSupportFragmentManager(), null);
    }
}
