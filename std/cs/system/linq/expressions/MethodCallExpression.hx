package cs.system.linq.expressions;

/** Represents a call to either static or an instance method. */
@:native("System.Linq.Expressions.MethodCallExpression")
extern class MethodCallExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets a collection of expressions that represent arguments of the called method.
	 * @return A  of  objects which represent the arguments to the called method.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the  for the method to be called.
	 * @return The  that represents the called method.
	 */
	var Method(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the  that represents the instance for instance method calls or null for
	 * static method calls.
	 * @return An  that represents the receiving object of the method.
	 */
	var Object(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param object The  property of the result.
	 * @param arguments The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(object:cs.system.linq.expressions.Expression, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression;
}
