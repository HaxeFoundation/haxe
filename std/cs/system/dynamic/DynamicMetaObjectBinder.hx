package cs.system.dynamic;

/** The dynamic call site binder that participates in the  binding protocol. */
@:native("System.Dynamic.DynamicMetaObjectBinder")
extern class DynamicMetaObjectBinder extends cs.system.runtime.compilerservices.CallSiteBinder {
	/**
	 * The result type of the operation.
	 * @return The  object representing the result type of the operation.
	 */
	var ReturnType(default, never):cs.system.Type;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * When overridden in the derived class, performs the binding of the dynamic
	 * operation.
	 * @param target The target of the dynamic operation.
	 * @param args An array of arguments of the dynamic operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(args:cs.NativeArray<Dynamic>, parameters:cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ParameterExpression>, returnLabel:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.Expression;
	@:overload(function(args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Defers the binding of the operation until later time when the runtime values of
	 * all dynamic operation arguments have been computed.
	 * @param target The target of the dynamic operation.
	 * @param args An array of arguments of the dynamic operation.
	 * @return The  representing the result of the binding.
	 */
	function Defer(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Gets an expression that will cause the binding to be updated. It indicates that
	 * the expression's binding is no longer valid. This is typically used when the
	 * "version" of a dynamic object has changed.
	 * @param type The  property of the resulting expression; any type is allowed.
	 * @return The update expression.
	 */
	function GetUpdateExpression(type:cs.system.Type):cs.system.linq.expressions.Expression;
}
