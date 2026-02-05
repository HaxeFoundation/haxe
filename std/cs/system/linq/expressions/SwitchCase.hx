package cs.system.linq.expressions;

/** Represents one case of a . */
@:native("System.Linq.Expressions.SwitchCase")
extern class SwitchCase {
	/**
	 * Gets the body of this case.
	 * @return The  object that represents the body of the case block.
	 */
	var Body(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the values of this case. This case is selected for execution when the 
	 * matches any of these values.
	 * @return The read-only collection of the values for this case block.
	 */
	var TestValues(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Returns a  that represents the current .
	 * @return A  that represents the current .
	 */
	function ToString():String;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param testValues The  property of the result.
	 * @param body The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(testValues:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>, body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.SwitchCase;
}
