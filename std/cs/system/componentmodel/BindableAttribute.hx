package cs.system.componentmodel;

/** Specifies whether a member is typically used for binding. This class cannot be inherited. */
@:native("System.ComponentModel.BindableAttribute")
extern class BindableAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is . This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.BindableAttribute;
	/** Specifies that a property is not typically used for binding. This field is read-only. */
	static var No(default, never):cs.system.componentmodel.BindableAttribute;
	/** Specifies that a property is typically used for binding. This field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.BindableAttribute;
	/**
	 * Gets a value indicating that a property is typically used for binding.
	 * @return if the property is typically used for binding; otherwise, .
	 */
	var Bindable(default, never):Bool;
	/**
	 * Gets a value indicating the direction or directions of this property's data
	 * binding.
	 * @return The direction of this property's data binding.
	 */
	var Direction(default, never):cs.system.componentmodel.BindingDirection;
	@:overload(function(bindable:Bool):Void {})
	@:overload(function(flags:cs.system.componentmodel.BindableSupport):Void {})
	@:overload(function(bindable:Bool, direction:cs.system.componentmodel.BindingDirection):Void {})
	function new(flags:cs.system.componentmodel.BindableSupport, direction:cs.system.componentmodel.BindingDirection):Void;
	/**
	 * Determines whether two  objects are equal.
	 * @param obj The object to compare.
	 * @return if the specified  is equal to the current ;  if it is not equal.
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for the  class.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
