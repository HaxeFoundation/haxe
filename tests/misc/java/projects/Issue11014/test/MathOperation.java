package test;

/**
 * A functional interface for performing
 * mathematical operations with two operands and one result.
 */
@FunctionalInterface
interface MathOperation {
	int perform(int a, int b);
}
