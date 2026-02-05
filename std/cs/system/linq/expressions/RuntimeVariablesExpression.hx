package cs.system.linq.expressions;

/** An expression that provides runtime read/write permission for variables. */
@:native("System.Linq.Expressions.RuntimeVariablesExpression")
extern class RuntimeVariablesExpression extends cs.system.linq.expressions.Expression {
	/**
	 * The variables or parameters to which to provide runtime access.
	 * @return The read-only collection containing parameters that will be provided the
	 * runtime access.
	 */
	var Variables(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ParameterExpression>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param variables The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.RuntimeVariablesExpression;
}
