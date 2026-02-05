package cs.system.dynamic;

/** Represents the invoke member dynamic operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.InvokeMemberBinder")
extern class InvokeMemberBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the signature of the arguments at the call site.
	 * @return The signature of the arguments at the call site.
	 */
	var CallInfo(default, never):cs.system.dynamic.CallInfo;
	/**
	 * Gets the value indicating if the string comparison should ignore the case of the
	 * member name.
	 * @return if case is ignored; otherwise, .
	 */
	var IgnoreCase(default, never):Bool;
	/**
	 * Gets the name of the member to invoke.
	 * @return The name of the member to invoke.
	 */
	var Name(default, never):String;
	/**
	 * Performs the binding of the dynamic invoke member operation.
	 * @param target The target of the dynamic invoke member operation.
	 * @param args An array of arguments of the dynamic invoke member operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * When overridden in the derived class, performs the binding of the dynamic invoke
	 * operation if the target dynamic object cannot bind.
	 * @param target The target of the dynamic invoke operation.
	 * @param args The arguments of the dynamic invoke operation.
	 * @param errorSuggestion The binding result to use if binding fails, or null.
	 * @return The  representing the result of the binding.
	 */
	function FallbackInvoke(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic invoke member operation if the target
	 * dynamic object cannot bind.
	 * @param target The target of the dynamic invoke member operation.
	 * @param args The arguments of the dynamic invoke member operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackInvokeMember(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
