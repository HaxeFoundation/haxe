package cs.system.linq.expressions;

/** Represents a control expression that handles multiple selections by passing control to . */
@:native("System.Linq.Expressions.SwitchExpression")
extern class SwitchExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the collection of  objects for the switch.
	 * @return The collection of  objects.
	 */
	var Cases(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.SwitchCase>;
	/**
	 * Gets the equality comparison method, if any.
	 * @return The  object representing the equality comparison method.
	 */
	var Comparison(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the test for the switch.
	 * @return The  object representing the test for the switch.
	 */
	var DefaultBody(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the test for the switch.
	 * @return The  object representing the test for the switch.
	 */
	var SwitchValue(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param switchValue The  property of the result.
	 * @param cases The  property of the result.
	 * @param defaultBody The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(switchValue:cs.system.linq.expressions.Expression, cases:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.SwitchCase>, defaultBody:cs.system.linq.expressions.Expression):cs.system.linq.expressions.SwitchExpression;
}
