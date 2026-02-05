package cs.system.componentmodel;

/** Indicates whether the component associated with this attribute has been inherited from a base class. This class cannot be inherited. */
@:native("System.ComponentModel.InheritanceAttribute")
extern class InheritanceAttribute extends cs.system.Attribute {
	/** Specifies that the default value for  is . This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.InheritanceAttribute;
	/** Specifies that the component is inherited. This field is read-only. */
	static var Inherited(default, never):cs.system.componentmodel.InheritanceAttribute;
	/** Specifies that the component is inherited and is read-only. This field is read-only. */
	static var InheritedReadOnly(default, never):cs.system.componentmodel.InheritanceAttribute;
	/** Specifies that the component is not inherited. This field is read-only. */
	static var NotInherited(default, never):cs.system.componentmodel.InheritanceAttribute;
	/**
	 * Gets or sets the current inheritance level stored in this attribute.
	 * @return The  stored in this attribute.
	 */
	var InheritanceLevel(default, never):cs.system.componentmodel.InheritanceLevel;
	@:overload(function():Void {})
	function new(inheritanceLevel:cs.system.componentmodel.InheritanceLevel):Void;
	/**
	 * Override to test for equality.
	 * @param value The object to test.
	 * @return if the object is the same; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hashcode for this object.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Gets a value indicating whether the current value of the attribute is the
	 * default value for the attribute.
	 * @return if the current value of the attribute is the default; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
	/**
	 * Converts this attribute to a string.
	 * @return A string that represents this .
	 */
	function ToString():String;
}
