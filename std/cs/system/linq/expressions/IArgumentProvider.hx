package cs.system.linq.expressions;

/** Provides an internal interface for accessing the arguments of multiple tree nodes (DynamicExpression, ElementInit, MethodCallExpression, InvocationExpression, NewExpression, and IndexExpression).  This API is for internal use only. */
@:native("System.Linq.Expressions.IArgumentProvider")
extern interface IArgumentProvider {
	/**
	 * Returns the number of arguments to the expression tree node. This API is for
	 * internal use only.
	 * @return The number of arguments to the expression tree node as .
	 */
	var ArgumentCount(default, never):Int;
	/**
	 * Returns the argument at , throwing if  is out of bounds. This API is for
	 * internal use only.
	 * @param index The index of the argument.
	 * @return The argument at index.
	 */
	function GetArgument(index:Int):cs.system.linq.expressions.Expression;
}
