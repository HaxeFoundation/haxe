package cs.system.linq.expressions;

/** Represents an expression that has a conditional operator. */
@:native("System.Linq.Expressions.ConditionalExpression")
extern class ConditionalExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the expression to execute if the test evaluates to .
	 * @return An  that represents the expression to execute if the test is .
	 */
	var IfFalse(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the expression to execute if the test evaluates to .
	 * @return An  that represents the expression to execute if the test is .
	 */
	var IfTrue(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the test of the conditional operation.
	 * @return An  that represents the test of the conditional operation.
	 */
	var Test(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression
	 * @param test The  property of the result.
	 * @param ifTrue The  property of the result.
	 * @param ifFalse The  property of the result.
	 * @return This expression if no children changed, or an expression with the
	 * updated children.
	 */
	function Update(test:cs.system.linq.expressions.Expression, ifTrue:cs.system.linq.expressions.Expression, ifFalse:cs.system.linq.expressions.Expression):cs.system.linq.expressions.ConditionalExpression;
}
