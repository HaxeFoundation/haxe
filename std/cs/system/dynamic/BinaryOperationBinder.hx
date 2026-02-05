package cs.system.dynamic;

/** Represents the binary dynamic operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.BinaryOperationBinder")
extern class BinaryOperationBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * The binary operation kind.
	 * @return The  object representing the kind of binary operation.
	 */
	var Operation(default, never):cs.system.linq.expressions.ExpressionType;
	/**
	 * Performs the binding of the dynamic binary operation.
	 * @param target The target of the dynamic operation.
	 * @param args An array of arguments of the dynamic operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject, arg:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the binary dynamic operation if the target dynamic
	 * object cannot bind.
	 * @param target The target of the dynamic binary operation.
	 * @param arg The right hand side operand of the dynamic binary operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackBinaryOperation(target:cs.system.dynamic.DynamicMetaObject, arg:cs.system.dynamic.DynamicMetaObject, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
