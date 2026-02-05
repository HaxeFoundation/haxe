package cs.system.dynamic;

/** Represents the invoke dynamic operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.InvokeBinder")
extern class InvokeBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the signature of the arguments at the call site.
	 * @return The signature of the arguments at the call site.
	 */
	var CallInfo(default, never):cs.system.dynamic.CallInfo;
	/**
	 * Performs the binding of the dynamic invoke operation.
	 * @param target The target of the dynamic invoke operation.
	 * @param args An array of arguments of the dynamic invoke operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic invoke operation if the target dynamic
	 * object cannot bind.
	 * @param target The target of the dynamic invoke operation.
	 * @param args The arguments of the dynamic invoke operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackInvoke(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
