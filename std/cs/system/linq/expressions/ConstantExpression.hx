package cs.system.linq.expressions;

/** Represents an expression that has a constant value. */
@:native("System.Linq.Expressions.ConstantExpression")
extern class ConstantExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the value of the constant expression.
	 * @return An  equal to the value of the represented expression.
	 */
	var Value(default, never):Dynamic;
}
