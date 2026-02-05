package cs.system.linq.expressions;

/** Describes a lambda expression. This captures a block of code that is similar to a .NET method body. */
@:native("System.Linq.Expressions.LambdaExpression")
extern class LambdaExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the body of the lambda expression.
	 * @return An  that represents the body of the lambda expression.
	 */
	var Body(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the name of the lambda expression.
	 * @return The name of the lambda expression.
	 */
	var Name(default, never):String;
	/**
	 * Gets the parameters of the lambda expression.
	 * @return A  of  objects that represent the parameters of the lambda expression.
	 */
	var Parameters(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ParameterExpression>;
	/**
	 * Gets the return type of the lambda expression.
	 * @return The  object representing the type of the lambda expression.
	 */
	var ReturnType(default, never):cs.system.Type;
	/**
	 * Gets the value that indicates if the lambda expression will be compiled with the
	 * tail call optimization.
	 * @return if the lambda expression will be compiled with the tail call
	 * optimization; otherwise, .
	 */
	var TailCall(default, never):Bool;
	@:overload(function():cs.system.Delegate {})
	@:overload(function(preferInterpretation:Bool):cs.system.Delegate {})
	/**
	 * Produces a delegate that represents the lambda expression.
	 * @return A  that contains the compiled version of the lambda expression.
	 */
	function Compile(debugInfoGenerator:cs.system.runtime.compilerservices.DebugInfoGenerator):cs.system.Delegate;
}
