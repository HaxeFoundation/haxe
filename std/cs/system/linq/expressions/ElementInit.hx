package cs.system.linq.expressions;

/** Represents an initializer for a single element of an  collection. */
@:native("System.Linq.Expressions.ElementInit")
extern class ElementInit {
	/**
	 * Gets the instance method that is used to add an element to an  collection.
	 * @return A  that represents an instance method that adds an element to a
	 * collection.
	 */
	var AddMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the collection of arguments that are passed to a method that adds an
	 * element to an  collection.
	 * @return A  of  objects that represent the arguments for a method that adds an
	 * element to a collection.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Returns a textual representation of an  object.
	 * @return A textual representation of the  object.
	 */
	function ToString():String;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param arguments The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ElementInit;
}
