package cs.system.componentmodel;

/** Specifies that an object has no subproperties capable of being edited. This class cannot be inherited. */
@:native("System.ComponentModel.ImmutableObjectAttribute")
extern class ImmutableObjectAttribute extends cs.system.Attribute {
	/** Represents the default value for . */
	static var Default(default, never):cs.system.componentmodel.ImmutableObjectAttribute;
	/** Specifies that an object has at least one editable subproperty. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.ImmutableObjectAttribute;
	/** Specifies that an object has no subproperties that can be edited. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.ImmutableObjectAttribute;
	/**
	 * Gets whether the object is immutable.
	 * @return if the object is immutable; otherwise, .
	 */
	var Immutable(default, never):Bool;
	function new(immutable:Bool):Void;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An  to compare with this instance or a null reference ( in Visual
	 * Basic).
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Indicates whether the value of this instance is the default value.
	 * @return if this instance is the default attribute for the class; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
