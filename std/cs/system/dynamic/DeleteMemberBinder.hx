package cs.system.dynamic;

/** Represents the dynamic delete member operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.DeleteMemberBinder")
extern class DeleteMemberBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * Gets the value indicating if the string comparison should ignore the case of the
	 * member name.
	 * @return if the string comparison should ignore case; otherwise, .
	 */
	var IgnoreCase(default, never):Bool;
	/**
	 * Gets the name of the member to delete.
	 * @return The name of the member to delete.
	 */
	var Name(default, never):String;
	/**
	 * Performs the binding of the dynamic delete member operation.
	 * @param target The target of the dynamic delete member operation.
	 * @param args An array of arguments of the dynamic delete member operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the dynamic delete member operation if the target
	 * dynamic object cannot bind.
	 * @param target The target of the dynamic delete member operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackDeleteMember(target:cs.system.dynamic.DynamicMetaObject, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
