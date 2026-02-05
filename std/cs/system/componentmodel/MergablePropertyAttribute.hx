package cs.system.componentmodel;

/** Specifies that this property can be combined with properties belonging to other objects in a Properties window. */
@:native("System.ComponentModel.MergablePropertyAttribute")
extern class MergablePropertyAttribute extends cs.system.Attribute {
	/** Specifies the default value, which is , that is a property can be combined with properties belonging to other objects in a Properties window. This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.MergablePropertyAttribute;
	/** Specifies that a property cannot be combined with properties belonging to other objects in a Properties window. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.MergablePropertyAttribute;
	/** Specifies that a property can be combined with properties belonging to other objects in a Properties window. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.MergablePropertyAttribute;
	/**
	 * Gets a value indicating whether this property can be combined with properties
	 * belonging to other objects in a Properties window.
	 * @return if this property can be combined with properties belonging to other
	 * objects in a Properties window; otherwise, .
	 */
	var AllowMerge(default, never):Bool;
	function new(allowMerge:Bool):Void;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj Another object to compare to.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
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
