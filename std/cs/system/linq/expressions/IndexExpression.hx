package cs.system.linq.expressions;

/** Represents indexing a property or array. */
@:native("System.Linq.Expressions.IndexExpression")
extern class IndexExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the arguments that will be used to index the property or array.
	 * @return The read-only collection containing the arguments that will be used to
	 * index the property or array.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the  for the property if the expression represents an indexed property,
	 * returns null otherwise.
	 * @return The  for the property if the expression represents an indexed property,
	 * otherwise null.
	 */
	var Indexer(default, never):cs.system.reflection.PropertyInfo;
	/**
	 * An object to index.
	 * @return The  representing the object to index.
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
	function Update(object:cs.system.linq.expressions.Expression, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression;
}
