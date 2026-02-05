package cs.system.dynamic;

/** Represents a dynamic object, that can have its operations bound at runtime. */
@:native("System.Dynamic.IDynamicMetaObjectProvider")
extern interface IDynamicMetaObjectProvider {
	/**
	 * Returns the  responsible for binding operations performed on this object.
	 * @param parameter The expression tree representation of the runtime value.
	 * @return The  to bind this object.
	 */
	function GetMetaObject(parameter:cs.system.linq.expressions.Expression):cs.system.dynamic.DynamicMetaObject;
}
