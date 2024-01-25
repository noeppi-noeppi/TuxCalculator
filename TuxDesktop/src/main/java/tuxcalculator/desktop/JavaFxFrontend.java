package tuxcalculator.desktop;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextAlignment;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import tuxcalculator.api.TuxCalculator;

import java.io.IOException;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

public class JavaFxFrontend extends GraphicalFrontend {

    private TextField in;
    private VBox out;
    private ScrollPane scrollPane;
    
    @Override
    public void init() {
        Platform.startup(() -> {});
    }

    private <T extends Event, E extends Exception> void runOnJfxThreadAndWait(ThrowingConsumer<EventHandler<T>, E> action) throws E {
        if (Platform.isFxApplicationThread()) {
            action.accept(null);
            return;
        }
        Object lock = new Object();
        AtomicReference<Exception> ex = new AtomicReference<>();
        synchronized (lock) {
            Platform.runLater(() -> {
                EventHandler<T> handler = event -> {
                    synchronized (lock) { lock.notify(); }
                };
                try {
                    action.accept(handler);
                } catch (Exception e) {
                    ex.set(e);
                    synchronized (lock) { lock.notify(); }
                }
            });
            try {
                lock.wait();
                Exception e = ex.get();
                if (e != null) {
                    //noinspection unchecked
                    throw (E) e;
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException(e);
            }
        }
    }
    
    @Override
    public void showError(String err) {
        this.<DialogEvent, RuntimeException>runOnJfxThreadAndWait(end -> {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setHeaderText("TuxCalculator - Error");
            alert.setContentText(err);
            alert.setResizable(true);
            if (end != null) {
                alert.setOnHidden(end);
                alert.show();
            } else {
                alert.showAndWait();
            }
        });
    }

    @Override
    public void run(TuxCalculator calc, Consumer<Callable<Void>> executor) throws IOException {
        super.run(calc, executor);
        
        URL res = JavaFxFrontend.class.getResource("/tuxcalculator/desktop/fx_window.fxml");
        if (res == null) throw new IOException("JavaFX window definition not found");
        
        this.<WindowEvent, IOException>runOnJfxThreadAndWait(end -> {
            Stage stage = new Stage();

            Parent root = FXMLLoader.load(res);
            stage.setTitle(Main.windowTitle());
            stage.setScene(new Scene(root, root.prefWidth(0), root.prefHeight(0)));

            this.in = (TextField) root.lookup("#tux_term_input");
            this.scrollPane = (ScrollPane) root.lookup("#tux_term_scroll");
            this.out = (VBox) this.scrollPane.getContent().lookup("#tux_term_output");
            Button enter = (Button) root.lookup("#tux_term_enter");

            this.in.addEventFilter(KeyEvent.KEY_PRESSED, event -> {
                switch (event.getCode()) {
                    case ENTER -> {
                        this.perform(Action.SUBMIT);
                        event.consume();
                    }
                    case UP, KP_UP -> {
                        this.perform(Action.INCR_HIST);
                        event.consume();
                    }
                    case DOWN, KP_DOWN -> {
                        this.perform(Action.DECR_HIST);
                        event.consume();
                    }
                    case TAB -> {
                        this.perform(event.isShiftDown() ? Action.TAB_BACKWARD : Action.TAB_FORWARD);
                        event.consume();
                    }
                }
            });
            
            enter.addEventFilter(ActionEvent.ACTION, event -> {
                this.perform(Action.SUBMIT);
                event.consume();
            });
            
            this.out.heightProperty().addListener((obs, ov, nv) -> this.scrollPane.setVvalue(this.scrollPane.getVmax()));
            this.out.widthProperty().addListener((obs, ov, nv) -> this.scrollPane.setVvalue(this.scrollPane.getVmax()));
            
            this.grabInputFocus();
            
            if (end != null) {
                stage.setOnHidden(end);
                stage.show();
            } else {
                stage.showAndWait();
            }
        });
    }

    @Override
    public void exit() {
        Platform.exit();
        super.exit();
    }

    @Override
    protected String getCurrentText() {
        return this.in.getText();
    }

    @Override
    protected void setCurrentText(String text) {
        this.in.setText(text);
    }

    @Override
    protected void grabInputFocus() {
        this.in.requestFocus();
    }

    @Override
    protected boolean hasSelectedText() {
        return !this.in.getSelectedText().isEmpty();
    }

    @Override
    protected int getCursorPosition() {
        return this.in.getCaretPosition();
    }

    @Override
    protected void placeCursorAt(int cursorPosition) {
        this.in.positionCaret(cursorPosition);
    }

    @Override
    protected void appendLine(String term, TuxCalculator.Result result) {
        Label input = new Label(term);
        input.setAlignment(Pos.BASELINE_LEFT);
        input.setTextAlignment(TextAlignment.LEFT);
        input.setWrapText(true);
        
        Label output = new Label(result.toString());
        output.setAlignment(Pos.BASELINE_RIGHT);
        output.setTextAlignment(TextAlignment.RIGHT);
        output.setWrapText(true);
        Insets padding = output.getPadding();
        output.setPadding(new Insets(padding.getTop(), padding.getRight(), padding.getBottom() + 2, padding.getLeft()));

        if (result instanceof TuxCalculator.Error err) {
            output.setStyle("-fx-font-weight:bold;-fx-text-fill:#DD0000;");
            if (!err.trace().isEmpty()) {
                output.setTooltip(new Tooltip(String.join("\n", err.trace())));
            }
        } else {
            output.setStyle("-fx-font-weight:bold;");
        }
        
        this.out.getChildren().addAll(this.wrapLine(input, false), this.wrapLine(output, true));
        
        this.scrollPane.setVvalue(this.scrollPane.getVmax());
    }
    
    private HBox wrapLine(Label line, boolean right) {
        line.setFocusTraversable(false);
        HBox box = new HBox();
        box.setFillHeight(false);
        box.setAlignment(right ? Pos.BASELINE_RIGHT : Pos.BASELINE_LEFT);
        box.getChildren().add(line);
        return box;
    }
    
    @FunctionalInterface
    private interface ThrowingConsumer<T, E extends Exception> {
        void accept(T value) throws E;
    }
}
