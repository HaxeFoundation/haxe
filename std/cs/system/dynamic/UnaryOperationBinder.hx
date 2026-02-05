package cs.system.dynamic;

/** Represents the unary dynamic operation at the call site, providing the binding semantic and the details about the operation. */
@:native("System.Dynamic.UnaryOperationBinder")
extern class UnaryOperationBinder extends cs.system.dynamic.DynamicMetaObjectBinder {
	/**
	 * The unary operation kind.
	 * @return The object of the  that represents the unary operation kind.
	 */
	var Operation(default, never):cs.system.linq.expressions.ExpressionType;
	/**
	 * Performs the binding of the dynamic unary operation.
	 * @param target The target of the dynamic operation.
	 * @param args An array of arguments of the dynamic operation.
	 * @return The  representing the result of the binding.
	 */
	function Bind(target:cs.system.dynamic.DynamicMetaObject, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	@:overload(function(target:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject {})
	/**
	 * Performs the binding of the unary dynamic operation if the target dynamic object
	 * cannot bind.
	 * @param target The target of the dynamic unary operation.
	 * @return The  representing the result of the binding.
	 */
	function FallbackUnaryOperation(target:cs.system.dynamic.DynamicMetaObject, errorSuggestion:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
}
