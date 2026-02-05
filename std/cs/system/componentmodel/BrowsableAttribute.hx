package cs.system.componentmodel;

/** Specifies whether a property or event should be displayed in a Properties window. */
@:native("System.ComponentModel.BrowsableAttribute")
extern class BrowsableAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.BrowsableAttribute;
	/** Specifies that a property or event cannot be modified at design time. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.BrowsableAttribute;
	/** Specifies that a property or event can be modified at design time. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.BrowsableAttribute;
	/**
	 * Gets a value indicating whether an object is browsable.
	 * @return if the object is browsable; otherwise, .
	 */
	var Browsable(default, never):Bool;
	function new(browsable:Bool):Void;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj Another object to compare to.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
