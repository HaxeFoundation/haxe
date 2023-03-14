package test;

public class Main {
    public static final MathOperation add = (a, b) -> a + b;
    public static final MathOperation subtract = (a, b) -> a - b;

    public static int performMathOperation(MathOperation operation) {
        return operation.perform(8, 4);
    }
}
