package cs.system.linq.expressions;

/** Represents a visitor or rewriter for expression trees. */
@:native("System.Linq.Expressions.ExpressionVisitor")
extern class ExpressionVisitor {
	/**
	 * Dispatches the list of expressions to one of the more specialized visit methods
	 * in this class.
	 * @param nodes The expressions to visit.
	 * @return The modified expression list, if any one of the elements were modified;
	 * otherwise, returns the original expression list.
	 */
	static function Visit<T>(nodes:cs.system.collections.objectmodel.ReadOnlyCollection<T>, elementVisitor:cs.system.Func_2<T, T>):cs.system.collections.objectmodel.ReadOnlyCollection<T>;
	@:overload(function(nodes:cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression> {})
	/**
	 * Dispatches the list of expressions to one of the more specialized visit methods
	 * in this class.
	 * @param nodes The expressions to visit.
	 * @return The modified expression list, if any one of the elements were modified;
	 * otherwise, returns the original expression list.
	 */
	function Visit(node:cs.system.linq.expressions.Expression):cs.system.linq.expressions.Expression;
	@:overload(function<T>(nodes:cs.system.collections.objectmodel.ReadOnlyCollection<T>, callerName:String):cs.system.collections.objectmodel.ReadOnlyCollection<T> {})
	/**
	 * Visits an expression, casting the result back to the original expression type.
	 * @param T The type of the expression.
	 * @param node The expression to visit.
	 * @param callerName The name of the calling method; used to report to report a
	 * better error message.
	 * @return The modified expression, if it or any subexpression was modified;
	 * otherwise, returns the original expression.
	 */
	function VisitAndConvert<T>(node:T, callerName:String):T;
}
