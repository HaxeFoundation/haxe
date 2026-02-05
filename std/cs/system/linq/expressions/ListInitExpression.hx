package cs.system.linq.expressions;

/** Represents a constructor call that has a collection initializer. */
@:native("System.Linq.Expressions.ListInitExpression")
extern class ListInitExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the element initializers that are used to initialize a collection.
	 * @return A  of  objects which represent the elements that are used to initialize
	 * the collection.
	 */
	var Initializers(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ElementInit>;
	/**
	 * Gets the expression that contains a call to the constructor of a collection
	 * type.
	 * @return A  that represents the call to the constructor of a collection type.
	 */
	var NewExpression(default, never):cs.system.linq.expressions.NewExpression;
	/**
	 * Reduces the binary expression node to a simpler expression.
	 * @return The reduced expression.
	 */
	function Reduce():cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param newExpression The  property of the result.
	 * @param initializers The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(newExpression:cs.system.linq.expressions.NewExpression, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.ListInitExpression;
}
