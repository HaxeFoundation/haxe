package cs.system.linq.expressions;

/** Represents a block that contains a sequence of expressions where variables can be defined. */
@:native("System.Linq.Expressions.BlockExpression")
extern class BlockExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the expressions in this block.
	 * @return The read-only collection containing all the expressions in this block.
	 */
	var Expressions(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the last expression in this block.
	 * @return The  object representing the last expression in this block.
	 */
	var Result(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the variables defined in this block.
	 * @return The read-only collection containing all the variables defined in this
	 * block.
	 */
	var Variables(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ParameterExpression>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param variables The  property of the result.
	 * @param expressions The  property of the result.
	 * @return This expression if no children changed, or an expression with the
	 * updated children.
	 */
	function Update(variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>, expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression;
}
