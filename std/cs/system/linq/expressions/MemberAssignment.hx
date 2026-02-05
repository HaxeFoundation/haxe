package cs.system.linq.expressions;

/** Represents assignment operation for a field or property of an object. */
@:native("System.Linq.Expressions.MemberAssignment")
extern class MemberAssignment extends cs.system.linq.expressions.MemberBinding {
	/**
	 * Gets the expression to assign to the field or property.
	 * @return The  that represents the value to assign to the field or property.
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param expression The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MemberAssignment;
}
