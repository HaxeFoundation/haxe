package cs.system.linq.expressions;

/** Represents an infinite loop. It can be exited with "break". */
@:native("System.Linq.Expressions.LoopExpression")
extern class LoopExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the  that is the body of the loop.
	 * @return The  that is the body of the loop.
	 */
	var Body(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the  that is used by the loop body as a break statement target.
	 * @return The  that is used by the loop body as a break statement target.
	 */
	var BreakLabel(default, never):cs.system.linq.expressions.LabelTarget;
	/**
	 * Gets the  that is used by the loop body as a continue statement target.
	 * @return The  that is used by the loop body as a continue statement target.
	 */
	var ContinueLabel(default, never):cs.system.linq.expressions.LabelTarget;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param breakLabel The  property of the result.
	 * @param continueLabel The  property of the result.
	 * @param body The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(breakLabel:cs.system.linq.expressions.LabelTarget, continueLabel:cs.system.linq.expressions.LabelTarget, body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.LoopExpression;
}
