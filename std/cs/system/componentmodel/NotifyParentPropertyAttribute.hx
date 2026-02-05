package cs.system.componentmodel;

/** Indicates that the parent property is notified when the value of the property that this attribute is applied to is modified. This class cannot be inherited. */
@:native("System.ComponentModel.NotifyParentPropertyAttribute")
extern class NotifyParentPropertyAttribute extends cs.system.Attribute {
	/** Indicates the default attribute state, that the property should not notify the parent property of changes to its value. This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.NotifyParentPropertyAttribute;
	/** Indicates that the parent property is not be notified of changes to the value of the property. This field is read-only. */
	static var No(default, never):cs.system.componentmodel.NotifyParentPropertyAttribute;
	/** Indicates that the parent property is notified of changes to the value of the property. This field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.NotifyParentPropertyAttribute;
	/**
	 * Gets or sets a value indicating whether the parent property should be notified
	 * of changes to the value of the property.
	 * @return if the parent property should be notified of changes; otherwise, .
	 */
	var NotifyParent(default, never):Bool;
	function new(notifyParent:Bool):Void;
	/**
	 * Gets a value indicating whether the specified object is the same as the current
	 * object.
	 * @param obj The object to test for equality.
	 * @return if the object is the same as this object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Gets the hash code for this object.
	 * @return The hash code for the object the attribute belongs to.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a value indicating whether the current value of the attribute is the
	 * default value for the attribute.
	 * @return if the current value of the attribute is the default value of the
	 * attribute; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
