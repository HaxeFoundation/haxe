package cs.system.linq.expressions;

/** Represents a label, which can be put in any  context. If it is jumped to, it will get the value provided by the corresponding . Otherwise, it receives the value in . If the  equals System.Void, no value should be provided. */
@:native("System.Linq.Expressions.LabelExpression")
extern class LabelExpression extends cs.system.linq.expressions.Expression {
	/**
	 * The value of the  when the label is reached through regular control flow (for
	 * example, is not jumped to).
	 * @return The Expression object representing the value of the .
	 */
	var DefaultValue(default, never):cs.system.linq.expressions.Expression;
	/**
	 * The  which this label is associated with.
	 * @return The  which this label is associated with.
	 */
	var Target(default, never):cs.system.linq.expressions.LabelTarget;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param target The  property of the result.
	 * @param defaultValue The  property of the result
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(target:cs.system.linq.expressions.LabelTarget, defaultValue:cs.system.linq.expressions.Expression):cs.system.linq.expressions.LabelExpression;
}
