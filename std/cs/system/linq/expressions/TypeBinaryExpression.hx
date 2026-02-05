package cs.system.linq.expressions;

/** Represents an operation between an expression and a type. */
@:native("System.Linq.Expressions.TypeBinaryExpression")
extern class TypeBinaryExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the expression operand of a type test operation.
	 * @return An  that represents the expression operand of a type test operation.
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the type operand of a type test operation.
	 * @return A  that represents the type operand of a type test operation.
	 */
	var TypeOperand(default, never):cs.system.Type;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param expression The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.TypeBinaryExpression;
}
