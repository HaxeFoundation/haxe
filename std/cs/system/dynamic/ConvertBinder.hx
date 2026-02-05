package cs.system.dynamic;

/** Represents the convert dynamic operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.ConvertBinder")
extern class ConvertBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the value indicating if the conversion should consider explicit
	 * conversions.
	 * @return if there is an explicit conversion; otherwise, .
	 */
	var Explicit(default, never):Bool;
	/**
	 * The type to convert to.
	 * @return The  object that represents the type to convert to.
	 */
	var Type(default, never):cs.system.Type;
	/**
	 * Performs the binding of the dynamic convert operation.
	 * @param target The target of the dynamic convert operation.
	 * @param args An array of arguments of the dynamic convert operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic convert operation if the target dynamic
	 * object cannot bind.
	 * @param target The target of the dynamic convert operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackConvert(target:cs.system.dynamic.DynamicMetaObject, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
