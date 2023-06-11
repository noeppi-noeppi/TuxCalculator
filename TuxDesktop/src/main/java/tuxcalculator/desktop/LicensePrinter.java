package tuxcalculator.desktop;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

public class LicensePrinter {
    
    public static void printLicenses() {
        printLicense("scala", "NOTICE-scala", "LICENSE-scala");
        printLicense("scala-parser-combinators", "NOTICE-scala-parser-combinators", "LICENSE-scala-parser-combinators");
        printLicense("commons-lang", "NOTICE-commons-lang", "LICENSE-commons-lang");
        printLicense("commons-io", "NOTICE-commons-io", "LICENSE-commons-io");
        printLicense("commons-text", "NOTICE-commons-text", "LICENSE-commons-text");
        printLicense("jsr305", "LICENSE-jsr305");
        printLicense("big-math", "LICENSE-big-math");
        printLicense("jansi", "LICENSE-jansi");
        printLicense("jline3", "LICENSE-jline3");
    }
    
    private static void printLicense(String name, String... files) {
        System.out.println("\n\n -- " + name + " --");
        if (files.length > 0) {
            System.out.println();
            for (String file : files) {
                try (InputStream in = LicensePrinter.class.getResourceAsStream("/tuxcalculator/notices/" + file)) {
                    if (in == null) throw new FileNotFoundException("License file not found: " + file);
                    in.transferTo(System.out);
                    System.out.println();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            System.out.println();
        }
    }
}
