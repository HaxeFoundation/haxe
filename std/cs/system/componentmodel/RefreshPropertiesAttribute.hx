package cs.system.componentmodel;

/** Indicates that the property grid should refresh when the associated property value changes. This class cannot be inherited. */
@:native("System.ComponentModel.RefreshPropertiesAttribute")
extern class RefreshPropertiesAttribute extends cs.system.Attribute {
	/** Indicates that all properties are queried again and refreshed if the property value is changed. This field is read-only. */
	static var All(default, never):cs.system.componentmodel.RefreshPropertiesAttribute;
	/** Indicates that no other properties are refreshed if the property value is changed. This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.RefreshPropertiesAttribute;
	/** Indicates that all properties are repainted if the property value is changed. This field is read-only. */
	static var Repaint(default, never):cs.system.componentmodel.RefreshPropertiesAttribute;
	/**
	 * Gets the refresh properties for the member.
	 * @return A  that indicates the current refresh properties for the member.
	 */
	var RefreshProperties(default, never):cs.system.componentmodel.RefreshProperties;
	function new(refresh:cs.system.componentmodel.RefreshProperties):Void;
	/**
	 * Overrides the object's  method.
	 * @param value The object to test for equality.
	 * @return if the specified object is the same; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this object.
	 * @return The hash code for the object that the attribute belongs to.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a value indicating whether the current value of the attribute is the
	 * default value for the attribute.
	 * @return if the current value of the attribute is the default; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
