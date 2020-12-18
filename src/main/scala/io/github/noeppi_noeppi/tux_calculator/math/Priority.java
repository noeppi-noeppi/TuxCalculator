package io.github.noeppi_noeppi.tux_calculator.math;

public enum Priority {
    COMPARISON("Vergleich"),
    ADDITIVE("Strichrechnung"),
    MULTIPLICATIVE("Punktrechnung"),
    POWER("Potenzrechnung");

    public final String rep;

    Priority(String rep) {
        this.rep = rep;
    }

    public boolean shouldEvaluateFirst(Priority other, Boolean isRightAssoc) {
        return isRightAssoc ? ordinal() < other.ordinal() : ordinal() <= other.ordinal();
    }
}
