package tuxcalculator.desktop;

import tuxcalculator.api.TuxCalculator;
import tuxcalculator.api.TuxFrontend;

import java.awt.*;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Locale;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

public abstract class DesktopFrontend implements TuxFrontend {
    
    public void init() {
        
    }
    
    public abstract void run(TuxCalculator calc, CalculatorHistory history, Consumer<Callable<Void>> executor) throws IOException;
    
    @Override
    public OutputStream openFile(String fileName) throws IOException {
        return Files.newOutputStream(Paths.get(fileName), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    @Override
    public void exit() {
        System.exit(0);
    }
    
    public static DesktopFrontend get(String id) {
        return switch (id.toLowerCase(Locale.ROOT)) {
            case "text" -> new TextFrontEnd();
            case "gtk" -> new GtkFrontend();
            case "jfx" -> new JavaFxFrontend();
            case "swing" -> new SwingFrontend();
            default -> throw new IllegalArgumentException("Unknown gui: " + id);
        };
    }
    
    public static DesktopFrontend auto() {
        if (GraphicsEnvironment.isHeadless() || System.console() != null) {
            return get("text");
        } else try {
            Class.forName("org.gnome.gtk.Gtk");
            return get("gtk");
        } catch (ClassNotFoundException e1) {try {
            Class.forName("javafx.application.Platform");
            return get("jfx");
        } catch (ClassNotFoundException e2) {
            return get("swing");
        }}
    }
}
