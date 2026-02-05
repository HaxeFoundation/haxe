package cs.system.linq.expressions;

/** Provides an internal interface for accessing the arguments of DynamicExpression tree nodes as well as CallSite and Rewriting functionality.  You should not use this API.  It is only public due to DLL refactoring and exists only for internal performance optimizations. */
@:native("System.Linq.Expressions.IDynamicExpression")
extern interface IDynamicExpression extends cs.system.linq.expressions.IArgumentProvider {
	/**
	 * Gets the delegate type used by the CallSite, which is the type of the rules used
	 * in the dynamic expression's polymorphic inline cache.
	 * @return The delegate type used by the CallSite.
	 */
	var DelegateType(default, never):cs.system.Type;
	/**
	 * Optionally creates the CallSite and returns the CallSite for the
	 * DynamicExpression's polymorphic inline cache.  You should not use this type.  It
	 * is only public due to assembly refactoring, and it is used internally for
	 * performance optimizations.
	 * @return The CallSite for the DynamicExpression's polymorphic inline cache.
	 */
	function CreateCallSite():Dynamic;
	/**
	 * Rewrites this node replacing the dynamic expression's arguments with the
	 * provided values.  The number of  needs to match the number of the current
	 * expression.  You should not use this type.  It is only public due to assembly
	 * refactoring, and it is used internally for performance optimizations.  This
	 * helper method allows re-writing of nodes to be independent of the specific
	 * implementation class deriving from DynamicExpression that is being used at the
	 * call site.
	 * @param args The arguments used to replace this node.
	 * @return The rewritten node, but if no changes were made, then returns the same
	 * node.
	 */
	function Rewrite(args:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.Expression;
}
