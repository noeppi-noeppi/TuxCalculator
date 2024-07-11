package tuxcalculator.desktop;

import joptsimple.OptionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;
import joptsimple.util.PathConverter;
import tuxcalculator.api.TuxCalculator;
import tuxcalculator.api.TuxCalculatorAPI;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class Main {

    private static String title = "This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION;
    private static String windowTitle = "TuxCalculator " + TuxCalculatorAPI.VERSION;
    
    public static void main(String[] args) throws IOException {
        Path defaultRcFile = Paths.get(System.getProperty("user.home")).resolve(".init.tuxc").toAbsolutePath().normalize();

        OptionParser options = new OptionParser(false);
        OptionSpec<Void> specIni = options.accepts("ini", "Run in ini-mode.");
        OptionSpec<String> specFmt = options.accepts("fmt", "Specify format to load.").withRequiredArg().defaultsTo("plain");
        OptionSpec<Void> specNoRc = options.accepts("no-rc", "Don't load the rc-file. Implicitly set by -ini.");
        OptionSpec<Path> specRcFile = options.accepts("rc-file", "The rc-file to load. (default: ~/.init.tuxc)").withRequiredArg().withValuesConvertedBy(new PathConverter());
        OptionSpec<Void> specHelp = options.accepts("help", "Show help.").forHelp();
        OptionSpec<Void> specVersion = options.accepts("version", "Print version information and exit.").forHelp();
        OptionSpec<Void> specLicenses = options.accepts("licenses", "Show open source licenses.").forHelp();
        OptionSpec<Void> specNoGui = options.accepts("nogui", "Equivalent to --gui=text");
        OptionSpec<String> specGui = options.accepts("gui", "The gui type to use. Default ist auto-detect.").withRequiredArg();

        OptionSet set;
        try {
            set = options.parse(args);
        } catch (OptionException e) {
            System.err.println("Failed to parse options: ");
            System.err.println("  " + e.getMessage().replace("\n", "\n  "));
            System.exit(2);
            throw e;
        }

        if (set.has(specIni)) {
            title = "This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION + " (INI)";
            windowTitle = "TuxCalculator " + TuxCalculatorAPI.VERSION + " (INI)";
        } else if (set.has(specFmt) && !Objects.equals(set.valueOf(specFmt), "plain")) {
            String shortFmt = set.valueOf(specFmt);
            if (shortFmt.contains("/")) shortFmt = shortFmt.substring(shortFmt.lastIndexOf('/') + 1);
            if (shortFmt.contains("\\")) shortFmt = shortFmt.substring(shortFmt.lastIndexOf('\\') + 1);
            title = "This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION + " (fmt=" + shortFmt + ")";
            windowTitle = "TuxCalculator " + TuxCalculatorAPI.VERSION;
        } else {
            title = "This is TuxCalculator, Version " + TuxCalculatorAPI.VERSION;
            windowTitle = "TuxCalculator " + TuxCalculatorAPI.VERSION;
        }
        
        if (set.has(specVersion) || set.has(specHelp) || set.has(specLicenses)) {
            System.out.println(title());
            if (set.has(specLicenses)) {
                LicensePrinter.printLicenses();
            }
            if (set.has(specHelp)) {
                System.out.println();
                options.printHelpOn(System.out);
            }
            return;
        }
        
        DesktopFrontend frontend;
        if (set.has(specNoGui)) {
            frontend = DesktopFrontend.get("text");
        } else if (set.has(specGui)) {
            frontend = DesktopFrontend.get(set.valueOf(specGui));
        } else if (set.has(specIni)) {
            frontend = DesktopFrontend.get("text");
        } else {
            frontend = DesktopFrontend.auto();
        }
        
        frontend.init();
        
        TuxCalculator calc;
        if (set.has(specIni)) {
            calc = TuxCalculatorAPI.get().createINI(frontend);
        } else {
            TuxCalculator.Builder builder;
            if (Objects.equals("plain", set.valueOf(specFmt))) {
                builder = TuxCalculatorAPI.get().createPlain(frontend);
            } else {
                builder = TuxCalculatorAPI.get().createBy(frontend, Paths.get(set.valueOf(specFmt) + ".tuxf"));
            }
            
            if (!set.has(specNoRc)) {
                if (set.has(specRcFile)) {
                  builder.load(set.valueOf(specRcFile).toAbsolutePath().normalize());
                } else if (Files.isRegularFile(defaultRcFile)) {
                  builder.load(defaultRcFile);
                }
            }
            
            List<TuxCalculator.Error> errors = builder.checkError();
            if (errors != null) {
                frontend.showError("There were errors initialising:\n" + errors.stream()
                        .map(err -> "  " + err.message())
                        .collect(Collectors.joining("\n"))
                );
                System.exit(1);
                return;
            }
            calc = builder.build();
        }
        
        Consumer<Callable<Void>> executor = action -> {
            try {
                action.call();
            } catch (Exception e) {
                StringWriter writer = new StringWriter();
                PrintWriter printer = new PrintWriter(writer);
                e.printStackTrace(printer);
                printer.close();
                frontend.showError("TuxCalculator encountered an error: " + e.getMessage() + "\n" + writer);
                System.exit(1);
            }
        };
        executor.accept(() -> {
            frontend.run(calc, executor);
            return null;
        });
        
        System.exit(0);
    }

    public static String title() {
        return Objects.requireNonNull(title);
    }

    public static String windowTitle() {
        return Objects.requireNonNull(windowTitle);
    }
}
