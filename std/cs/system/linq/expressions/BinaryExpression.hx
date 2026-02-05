package cs.system.linq.expressions;

/** Represents an expression that has a binary operator. */
@:native("System.Linq.Expressions.BinaryExpression")
extern class BinaryExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the type conversion function that is used by a coalescing or compound
	 * assignment operation.
	 * @return A  that represents a type conversion function.
	 */
	var Conversion(default, never):cs.system.linq.expressions.LambdaExpression;
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
	 * Gets the left operand of the binary operation.
	 * @return An  that represents the left operand of the binary operation.
	 */
	var Left(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the implementing method for the binary operation.
	 * @return The  that represents the implementing method.
	 */
	var Method(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the right operand of the binary operation.
	 * @return An  that represents the right operand of the binary operation.
	 */
	var Right(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Reduces the binary expression node to a simpler expression.
	 * @return The reduced expression.
	 */
	function Reduce():cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param left The  property of the result.
	 * @param conversion The  property of the result.
	 * @param right The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(left:cs.system.linq.expressions.Expression, conversion:cs.system.linq.expressions.LambdaExpression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression;
}
