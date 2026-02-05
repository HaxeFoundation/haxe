package cs.system.dynamic;

/** Represents the dynamic binding and a binding logic of an object participating in the dynamic binding. */
@:native("System.Dynamic.DynamicMetaObject")
extern class DynamicMetaObject {
	/** Represents an empty array of type . This field is read only. */
	static var EmptyMetaObjects(default, never):cs.NativeArray<cs.system.dynamic.DynamicMetaObject>;
	/**
	 * The expression representing the  during the dynamic binding process.
	 * @return The expression representing the  during the dynamic binding process.
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets a value indicating whether the  has the runtime value.
	 * @return if the  has the runtime value; otherwise, .
	 */
	var HasValue(default, never):Bool;
	/**
	 * Gets the limit type of the .
	 * @return if runtime value is available, a type of the  otherwise.
	 */
	var LimitType(default, never):cs.system.Type;
	/**
	 * The set of binding restrictions under which the binding is valid.
	 * @return The set of binding restrictions.
	 */
	var Restrictions(default, never):cs.system.dynamic.BindingRestrictions;
	/**
	 * Gets the  of the runtime value or null if the  has no value associated with it.
	 * @return The  of the runtime value or null.
	 */
	var RuntimeType(default, never):cs.system.Type;
	/**
	 * The runtime value represented by this .
	 * @return The runtime value represented by this .
	 */
	var Value(default, never):Dynamic;
	@:overload(function(expression:cs.system.linq.expressions.Expression, restrictions:cs.system.dynamic.BindingRestrictions):Void {})
	function new(expression:cs.system.linq.expressions.Expression, restrictions:cs.system.dynamic.BindingRestrictions, value:Dynamic):Void;
	/**
	 * Creates a meta-object for the specified object.
	 * @param value The object to get a meta-object for.
	 * @param expression The expression representing this  during the dynamic binding
	 * process.
	 * @return If the given object implements  and is not a remote object from outside
	 * the current AppDomain, returns the object's specific meta-object returned by .
	 * Otherwise a plain new meta-object with no restrictions is created and returned.
	 */
	static function Create(value:Dynamic, expression:cs.system.linq.expressions.Expression):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic binary operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param arg An instance of the  representing the right hand side of the binary
	 * operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindBinaryOperation(binder:cs.system.dynamic.BinaryOperationBinder, arg:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic conversion operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindConvert(binder:cs.system.dynamic.ConvertBinder):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic create instance operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param args An array of  instances - arguments to the create instance operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindCreateInstance(binder:cs.system.dynamic.CreateInstanceBinder, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic delete index operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param indexes An array of  instances - indexes for the delete index operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindDeleteIndex(binder:cs.system.dynamic.DeleteIndexBinder, indexes:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic delete member operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindDeleteMember(binder:cs.system.dynamic.DeleteMemberBinder):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic get index operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param indexes An array of  instances - indexes for the get index operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindGetIndex(binder:cs.system.dynamic.GetIndexBinder, indexes:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic get member operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindGetMember(binder:cs.system.dynamic.GetMemberBinder):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic invoke operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param args An array of  instances - arguments to the invoke operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindInvoke(binder:cs.system.dynamic.InvokeBinder, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic invoke member operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param args An array of  instances - arguments to the invoke member operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindInvokeMember(binder:cs.system.dynamic.InvokeMemberBinder, args:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic set index operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param indexes An array of  instances - indexes for the set index operation.
	 * @param value The  representing the value for the set index operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindSetIndex(binder:cs.system.dynamic.SetIndexBinder, indexes:cs.NativeArray<cs.system.dynamic.DynamicMetaObject>, value:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic set member operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @param value The  representing the value for the set member operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindSetMember(binder:cs.system.dynamic.SetMemberBinder, value:cs.system.dynamic.DynamicMetaObject):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Performs the binding of the dynamic unary operation.
	 * @param binder An instance of the  that represents the details of the dynamic
	 * operation.
	 * @return The new  representing the result of the binding.
	 */
	function BindUnaryOperation(binder:cs.system.dynamic.UnaryOperationBinder):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Returns the enumeration of all dynamic member names.
	 * @return The list of dynamic member names.
	 */
	function GetDynamicMemberNames():cs.system.collections.generic.IEnumerable<String>;
}
