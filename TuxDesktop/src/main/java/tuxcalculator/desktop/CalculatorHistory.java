package tuxcalculator.desktop;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CalculatorHistory {
    
    @Nullable private final Path path;
    private final int persistentSize;
    private final List<String> oldLines;
    private final ArrayList<String> newLines;

    public CalculatorHistory() {
        this.path = null;
        this.persistentSize = 0;
        this.oldLines = List.of();
        this.newLines = new ArrayList<>();
    }
    
    public CalculatorHistory(@Nonnull Path path, int persistentSize) {
        List<String> oldLines = new ArrayList<>();
        try {
            if (Files.isRegularFile(path)) {
                for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
                    if (oldLines.isEmpty() || !Objects.equals(line, oldLines.getLast())) {
                        oldLines.add(line);
                    }
                }
            } else {
                oldLines = List.of();
            }
        } catch (IOException e) {
            path = null;
            oldLines.clear();
        }
        this.path = path;
        this.persistentSize = this.path == null ? 0 : persistentSize;
        this.oldLines = List.copyOf(oldLines);
        this.newLines = new ArrayList<>();
    }

    public synchronized boolean isEmpty() {
        return this.oldLines.isEmpty() && this.newLines.isEmpty();
    }
    
    public synchronized int length() {
        return this.oldLines.size() + this.newLines.size();
    }
    
    public synchronized String get(int idx) {
        if (idx < 0) {
            throw new IndexOutOfBoundsException();
        } else if (idx < this.newLines.size()) {
            return this.newLines.get(this.newLines.size() - idx - 1);
        } else if ((idx - this.newLines.size()) < this.oldLines.size()) {
            return this.oldLines.get(this.oldLines.size() - (idx - this.newLines.size()) - 1);
        } else {
            throw new IndexOutOfBoundsException();
        }
    }
    
    public synchronized void add(String line) {
        if (this.isEmpty() || !line.equals(this.get(0))) {
            this.newLines.add(line);
        }
    }
    
    public synchronized void save() {
        if (this.path != null && this.persistentSize > 0) try {
            Path parent = this.path.toAbsolutePath().getParent();
            if (parent != null && !Files.exists(parent)) Files.createDirectories(parent);
            List<String> readLines = Files.isRegularFile(this.path) ? Files.readAllLines(this.path, StandardCharsets.UTF_8) : List.of();
            String newContent = Stream.concat(readLines.stream(), this.newLines.stream())
                    .skip(Math.max(0, readLines.size() + this.newLines.size() - this.persistentSize))
                    .map(line -> line.stripTrailing() + "\n")
                    .collect(Collectors.joining());
            Files.writeString(this.path, newContent, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
        } catch (IOException e) {
            //
        }
    }
}
