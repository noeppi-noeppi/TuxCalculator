package io.github.noeppi_noeppi.tux_calculator.math;

public enum Priority {
    COMPARISON("comparative", "Vergleich"),
    ELEMENT("element", "Element"),
    ADDITIVE("additive", "Strichrechnung"),
    MULTIPLICATIVE("multiplicative", "Punktrechnung"),
    POWER("power", "Potenzrechnung");

    public final String id;
    public final String rep;

    Priority(String id, String rep) {
        this.id = id;
        this.rep = rep;
    }

    public boolean shouldEvaluateFirst(Priority other, Boolean isRightAssoc) {
        return isRightAssoc ? ordinal() < other.ordinal() : ordinal() <= other.ordinal();
    }
}
