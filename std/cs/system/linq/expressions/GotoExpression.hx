package cs.system.linq.expressions;

/** Represents an unconditional jump. This includes return statements, break and continue statements, and other jumps. */
@:native("System.Linq.Expressions.GotoExpression")
extern class GotoExpression extends cs.system.linq.expressions.Expression {
	/**
	 * The kind of the "go to" expression. Serves information purposes only.
	 * @return The  object representing the kind of the "go to" expression.
	 */
	var Kind(default, never):cs.system.linq.expressions.GotoExpressionKind;
	/**
	 * The target label where this node jumps to.
	 * @return The  object representing the target label for this node.
	 */
	var Target(default, never):cs.system.linq.expressions.LabelTarget;
	/**
	 * The value passed to the target, or null if the target is of type System.Void.
	 * @return The  object representing the value passed to the target or null.
	 */
	var Value(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param target The  property of the result.
	 * @param value The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression):cs.system.linq.expressions.GotoExpression;
}
