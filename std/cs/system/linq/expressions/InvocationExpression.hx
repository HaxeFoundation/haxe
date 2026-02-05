package cs.system.linq.expressions;

/** Represents an expression that applies a delegate or lambda expression to a list of argument expressions. */
@:native("System.Linq.Expressions.InvocationExpression")
extern class InvocationExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the arguments that the delegate or lambda expression is applied to.
	 * @return A  of  objects which represent the arguments that the delegate is
	 * applied to.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the delegate or lambda expression to be applied.
	 * @return An  that represents the delegate to be applied.
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param expression The  property of the result.
	 * @param arguments The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(expression:cs.system.linq.expressions.Expression, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.InvocationExpression;
}
