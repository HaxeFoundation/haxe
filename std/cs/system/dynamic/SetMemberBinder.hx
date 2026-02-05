package cs.system.dynamic;

/** Represents the dynamic set member operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.SetMemberBinder")
extern class SetMemberBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the value indicating if the string comparison should ignore the case of the
	 * member name.
	 * @return if case is ignored; otherwise, .
	 */
	var IgnoreCase(default, never):Bool;
	/**
	 * Gets the name of the member to obtain.
	 * @return The name of the member to obtain.
	 */
	var Name(default, never):String;
	/**
	 * Performs the binding of the dynamic set member operation.
	 * @param target The target of the dynamic set member operation.
	 * @param args An array of arguments of the dynamic set member operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, value:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic set member operation if the target dynamic
	 * object cannot bind.
	 * @param target The target of the dynamic set member operation.
	 * @param value The value to set to the member.
	 * @return The  representing the result of the binding.
	 */
	function FallbackSetMember(target:cs.system.dynamic.DynamicMetaObject, value:cs.system.dynamic.DynamicMetaObject, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
