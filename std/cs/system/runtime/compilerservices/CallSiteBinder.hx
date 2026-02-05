package cs.system.runtime.compilerservices;

/** Class responsible for runtime binding of the dynamic operations on the dynamic call site. */
@:native("System.Runtime.CompilerServices.CallSiteBinder")
extern class CallSiteBinder {
	/**
	 * Gets a label that can be used to cause the binding to be updated. It indicates
	 * that the expression's binding is no longer valid. This is typically used when
	 * the "version" of a dynamic object has changed.
	 * @return The  object representing a label that can be used to trigger the binding
	 * update.
	 */
	static var UpdateLabel(default, never):cs.system.linq.expressions.LabelTarget;
	/**
	 * Performs the runtime binding of the dynamic operation on a set of arguments.
	 * @param args An array of arguments to the dynamic operation.
	 * @param parameters The array of  instances that represent the parameters of the
	 * call site in the binding process.
	 * @param returnLabel A LabelTarget used to return the result of the dynamic
	 * binding.
	 * @return An Expression that performs tests on the dynamic operation arguments,
	 * and performs the dynamic operation if the tests are valid. If the tests fail on
	 * subsequent occurrences of the dynamic operation, Bind will be called again to
	 * produce a new  for the new argument types.
	 */
	function Bind(args:cs.NativeArray<Dynamic>, parameters:cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ParameterExpression>, returnLabel:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.Expression;
	/**
	 * Provides low-level runtime binding support. Classes can override this and
	 * provide a direct delegate for the implementation of rule. This can enable saving
	 * rules to disk, having specialized rules available at runtime, or providing a
	 * different caching policy.
	 * @param T The target type of the CallSite.
	 * @param site The CallSite the bind is being performed for.
	 * @param args The arguments for the binder.
	 * @return A new delegate which replaces the CallSite Target.
	 */
	function BindDelegate<T>(site:cs.system.runtime.compilerservices.CallSite_1<T>, args:cs.NativeArray<Dynamic>):T;
}
