package cs.system.componentmodel;

/** Specifies that a list can be used as a data source. A visual designer should use this attribute to determine whether to display a particular list in a data-binding picker. This class cannot be inherited. */
@:native("System.ComponentModel.ListBindableAttribute")
extern class ListBindableAttribute extends cs.system.Attribute {
	/** Represents the default value for . */
	static var Default(default, never):cs.system.componentmodel.ListBindableAttribute;
	/** Specifies that the list is not bindable. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.ListBindableAttribute;
	/** Specifies that the list is bindable. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.ListBindableAttribute;
	/**
	 * Gets whether the list is bindable.
	 * @return if the list is bindable; otherwise, .
	 */
	var ListBindable(default, never):Bool;
	@:overload(function(listBindable:Bool):Void {})
	function new(flags:cs.system.componentmodel.BindableSupport):Void;
	/**
	 * Returns whether the object passed is equal to this .
	 * @param obj The object to test equality with.
	 * @return if the object passed is equal to this ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns whether  is set to the default value.
	 * @return if  is set to the default value; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
