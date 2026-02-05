package cs.system.linq.expressions;

/** Represents a try/catch/finally/fault block. */
@:native("System.Linq.Expressions.TryExpression")
extern class TryExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the  representing the body of the try block.
	 * @return The  representing the body of the try block.
	 */
	var Body(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the  representing the fault block.
	 * @return The  representing the fault block.
	 */
	var Fault(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the  representing the finally block.
	 * @return The  representing the finally block.
	 */
	var Finally(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the collection of  expressions associated with the try block.
	 * @return The collection of  expressions associated with the try block.
	 */
	var Handlers(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.CatchBlock>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param body The  property of the result.
	 * @param handlers The  property of the result.
	 * @param finally The  property of the result.
	 * @param fault The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(body:cs.system.linq.expressions.Expression, handlers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.CatchBlock>, finally:cs.system.linq.expressions.Expression, fault:cs.system.linq.expressions.Expression):cs.system.linq.expressions.TryExpression;
}
