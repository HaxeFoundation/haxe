package cs.system.linq.expressions;

/** Represents a constructor call. */
@:native("System.Linq.Expressions.NewExpression")
extern class NewExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the arguments to the constructor.
	 * @return A collection of  objects that represent the arguments to the
	 * constructor.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the called constructor.
	 * @return The  that represents the called constructor.
	 */
	var Constructor(default, never):cs.system.reflection.ConstructorInfo;
	/**
	 * Gets the members that can retrieve the values of the fields that were
	 * initialized with constructor arguments.
	 * @return A collection of  objects that represent the members that can retrieve
	 * the values of the fields that were initialized with constructor arguments.
	 */
	var Members(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.reflection.MemberInfo>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param arguments The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewExpression;
}
