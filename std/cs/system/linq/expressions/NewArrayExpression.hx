package cs.system.linq.expressions;

/** Represents creating a new array and possibly initializing the elements of the new array. */
@:native("System.Linq.Expressions.NewArrayExpression")
extern class NewArrayExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the bounds of the array if the value of the  property is , or the values to
	 * initialize the elements of the new array if the value of the  property is .
	 * @return A  of  objects which represent either the bounds of the array or the
	 * initialization values.
	 */
	var Expressions(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param expressions The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewArrayExpression;
}
