package cs.system.linq.expressions;

/** Represents an expression that has a unary operator. */
@:native("System.Linq.Expressions.UnaryExpression")
extern class UnaryExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets a value that indicates whether the expression tree node represents a lifted
	 * call to an operator.
	 * @return if the node represents a lifted call; otherwise, .
	 */
	var IsLifted(default, never):Bool;
	/**
	 * Gets a value that indicates whether the expression tree node represents a lifted
	 * call to an operator whose return type is lifted to a nullable type.
	 * @return if the operator's return type is lifted to a nullable type; otherwise, .
	 */
	var IsLiftedToNull(default, never):Bool;
	/**
	 * Gets the implementing method for the unary operation.
	 * @return The  that represents the implementing method.
	 */
	var Method(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the operand of the unary operation.
	 * @return An  that represents the operand of the unary operation.
	 */
	var Operand(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Reduces the expression node to a simpler expression.
	 * @return The reduced expression.
	 */
	function Reduce():cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param operand The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(operand:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression;
}
