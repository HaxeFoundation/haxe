package cs.system.linq.expressions;

/** Represents accessing a field or property. */
@:native("System.Linq.Expressions.MemberExpression")
extern class MemberExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the containing object of the field or property.
	 * @return An  that represents the containing object of the field or property.
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the field or property to be accessed.
	 * @return The  that represents the field or property to be accessed.
	 */
	var Member(default, never):cs.system.reflection.MemberInfo;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param expression The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MemberExpression;
}
