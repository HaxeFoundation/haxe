package cs.system.linq.expressions;

/** Represents calling a constructor and initializing one or more members of the new object. */
@:native("System.Linq.Expressions.MemberInitExpression")
extern class MemberInitExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the bindings that describe how to initialize the members of the newly
	 * created object.
	 * @return A  of  objects which describe how to initialize the members.
	 */
	var Bindings(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.MemberBinding>;
	/**
	 * Gets the expression that represents the constructor call.
	 * @return A  that represents the constructor call.
	 */
	var NewExpression(default, never):cs.system.linq.expressions.NewExpression;
	/**
	 * Reduces the  to a simpler expression.
	 * @return The reduced expression.
	 */
	function Reduce():cs.system.linq.expressions.Expression;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param newExpression The  property of the result.
	 * @param bindings The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(newExpression:cs.system.linq.expressions.NewExpression, bindings:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberInitExpression;
}
