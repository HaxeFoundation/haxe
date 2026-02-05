package cs.system.linq.expressions;

/** Represents a catch statement in a try block. */
@:native("System.Linq.Expressions.CatchBlock")
extern class CatchBlock {
	/**
	 * Gets the body of the catch block.
	 * @return The  object representing the catch body.
	 */
	var Body(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the body of the  filter.
	 * @return The  object representing the body of the  filter.
	 */
	var Filter(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the type of  this handler catches.
	 * @return The  object representing the type of  this handler catches.
	 */
	var Test(default, never):cs.system.Type;
	/**
	 * Gets a reference to the  object caught by this handler.
	 * @return The  object representing a reference to the  object caught by this
	 * handler.
	 */
	var Variable(default, never):cs.system.linq.expressions.ParameterExpression;
	/**
	 * Returns a  that represents the current .
	 * @return A  that represents the current .
	 */
	function ToString():String;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param variable The  property of the result.
	 * @param filter The  property of the result.
	 * @param body The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(variable:cs.system.linq.expressions.ParameterExpression, filter:cs.system.linq.expressions.Expression, body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock;
}
