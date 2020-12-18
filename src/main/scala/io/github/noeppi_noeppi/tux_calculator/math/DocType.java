package io.github.noeppi_noeppi.tux_calculator.math;

public enum DocType {
    GENERAL("Speziell"),
    CONSTANT("Konstante"),
    FUNCTION("Funktion"),
    OPERATOR("Operator"),
    UNARY("Vorzeichen"),
    POSTFIX("Nachgestellter Operator");

    public final String rep;

    DocType(String rep) {
        this.rep = rep;
    }
}
