package cs.system.dynamic;

/** Represents the dynamic get index operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.GetIndexBinder")
extern class GetIndexBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the signature of the arguments at the call site.
	 * @return The signature of the arguments at the call site.
	 */
	var CallInfo(default, never):cs.system.dynamic.CallInfo;
	/**
	 * Performs the binding of the dynamic get index operation.
	 * @param target The target of the dynamic get index operation.
	 * @param args An array of arguments of the dynamic get index operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, indexes:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic get index operation if the target dynamic
	 * object cannot bind.
	 * @param target The target of the dynamic get index operation.
	 * @param indexes The arguments of the dynamic get index operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackGetIndex(target:cs.system.dynamic.DynamicMetaObject, indexes:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
