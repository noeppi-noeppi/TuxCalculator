package tuxcalculator.android;

import android.app.Activity;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import androidx.annotation.Nullable;
import tuxcalculator.android.data.TextEntry;
import tuxcalculator.api.TuxCalculator;
import tuxcalculator.api.TuxCalculatorAPI;
import tuxcalculator.api.TuxFrontend;

import java.io.*;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.stream.Collectors;

public class ApplicationState {
    
    private static ApplicationState state = null;
    
    public static ApplicationState get(MainActivity activity) {
        if (!activity.getMainLooper().isCurrentThread()) throw new IllegalStateException("Thread error");
        if (state == null) state = new ApplicationState();
        state.update(activity);
        return state;
    }
    
    public static void finish() {
        if (state != null) {
            state.executor.shutdownNow();
            state = null;
        }
    }

    @Nullable private File mediaDir = null;
    private WeakReference<MainActivity> activity;
    private final List<TextEntry> text;
    private final ScheduledExecutorService executor;
    private TuxCalculator calculator;
    private TuxFrontend frontend;
    
    private String termInput;
    @Nullable private String lastTerm;
    @Nullable private String errorFailure;
    
    private ApplicationState() {
        this.text = new ArrayList<>();
        this.executor = new ScheduledThreadPoolExecutor(1);
        this.termInput = "";
    }
    
    private void update(MainActivity activity) {
        this.activity = new WeakReference<>(activity);
        File[] mediaDirs = activity.getExternalMediaDirs();
        this.mediaDir = mediaDirs.length == 0 ? null : mediaDirs[0];
        
        if (errorFailure != null) {
            activity.showErrorState(errorFailure);
            return;
        }
        
        if (calculator != null) {
            this.initLogic(activity);
        } else {
            activity.startLoad();
            this.executor.execute(() -> runProtected(activity, () -> {
                TuxCalculator.Builder builder;
                this.frontend = new Frontend();
                if (mediaDir != null) {
                    File formatFile = new File(mediaDir, "init.tuxf");
                    File rcFile = new File(mediaDir, "init.tuxc");

                    if (formatFile.isFile()) {
                        builder = TuxCalculatorAPI.get().createBy(frontend, new FileInputStream(formatFile));
                    } else {
                        builder = TuxCalculatorAPI.get().createPlain(frontend);
                    }

                    if (rcFile.isFile()) {
                        builder.load(rcFile.getName(), new FileInputStream(rcFile));
                    }
                } else {
                    builder = TuxCalculatorAPI.get().createPlain(frontend);
                }
                List<String> errors = builder.checkError();
                if (errors != null) {
                    this.makeErrorFail(activity, "There were errors initialising:\n" + errors.stream().map(err -> "  " + err).collect(Collectors.joining("\n")));
                } else {
                    this.calculator = builder.build();
                    activity.getMainExecutor().execute(() -> {
                        activity.endLoad();
                        this.initLogic(activity);
                    });
                }
                return null;
            }));
        }
    }
    
    private void initLogic(MainActivity activity) {
        if (this.calculator == null || this.frontend == null) throw new IllegalStateException("logic without calculator");
        runProtected(activity, () -> {
            for (TextEntry entry : this.text) {
                activity.addTextView(entry);
            }
            
            Button submit = activity.findViewById(R.id.submit);
            submit.setOnClickListener(v -> this.calcTerm(activity));
            
            AutoCompleteTextView input = activity.findViewById(R.id.term_input);
            input.setText(this.termInput);
            input.setAdapter(new TabCompletionAdapter(activity, calculator, input));
            input.setOnEditorActionListener((v, actionId, event) -> {
                this.calcTerm(activity);
                return true;
            });
            
            return null;
        });
    }
    
    public void updateTermInput(String currentInput) {
        this.termInput = currentInput;
    }
    
    private void runProtected(MainActivity activity, Callable<Void> action) {
        try {
            action.call();
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);
            pw.close();
            this.makeErrorFail(activity, "Fatal error:\n" + sw);
        }
    }
    
    private void makeErrorFail(MainActivity activity, String failure) {
        this.errorFailure = failure;
        activity.getMainExecutor().execute(() -> activity.showErrorState(this.errorFailure));
    }
    
    private void calcTerm(MainActivity activity) {
        AutoCompleteTextView input = activity.findViewById(R.id.term_input);
        String term = input.getText().toString();
        input.setText("");
        this.termInput = "";
        input.dismissDropDown();

        if (term.isBlank()) term = Objects.requireNonNullElse(this.lastTerm, "");
        else this.lastTerm = term;

        if (term.isBlank()) return;

        String effectiveFinalTerm = term;
        this.executor.execute(() -> {
            try {
                TuxCalculator.Result result = this.calculator.parse(effectiveFinalTerm);
                activity.getMainExecutor().execute(() -> {
                    TextEntry inputText = new TextEntry(effectiveFinalTerm, false, true, false, null);
                    String detail = null;
                    if (result instanceof TuxCalculator.Error err) {
                        detail = err.trace().isEmpty() ? "" : String.join("\n", err.trace());
                    }
                    TextEntry outputText = new TextEntry(result.toString(), true, true, result instanceof TuxCalculator.Error, detail);

                    this.text.add(inputText);
                    this.text.add(outputText);

                    activity.addTextView(inputText);
                    activity.addTextView(outputText);
                });
            } catch (Exception e) {
                this.frontend.showError("TuxCalculator encountered an error: " + e.getMessage());
            }
        });
    }
    
    @Nullable
    public MainActivity getActivity() {
        MainActivity activity = this.activity.get();
        if (activity != null && !activity.isFinishing()) return activity;
        return null;
    }
    
    private class Frontend implements TuxFrontend {

        @Override
        public void showError(String err) {
            MainActivity activity = ApplicationState.this.getActivity();
            if (activity != null) activity.getMainExecutor().execute(() -> activity.showErrorPopup(err));
        }

        @Override
        public OutputStream openFile(String fileName) throws IOException {
            if (ApplicationState.this.mediaDir == null) throw new IOException("No media dir found");
            File targetFile = new File(ApplicationState.this.mediaDir, fileName);
            return new BufferedOutputStream(new FileOutputStream(targetFile, false));
        }

        @Override
        public void exit() {
            Activity activity = ApplicationState.this.getActivity();
            if (activity != null) activity.getMainExecutor().execute(activity::finish);
        }
    }
}
