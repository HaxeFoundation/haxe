package cs.system.componentmodel;

/** Specifies when a component property can be bound to an application setting. */
@:native("System.ComponentModel.SettingsBindableAttribute")
extern class SettingsBindableAttribute extends cs.system.Attribute {
	/** Specifies that a property is not appropriate to bind settings to. */
	static var No(default, never):cs.system.componentmodel.SettingsBindableAttribute;
	/** Specifies that a property is appropriate to bind settings to. */
	static var Yes(default, never):cs.system.componentmodel.SettingsBindableAttribute;
	/**
	 * Gets a value indicating whether a property is appropriate to bind settings to.
	 * @return if the property is appropriate to bind settings to; otherwise, .
	 */
	var Bindable(default, never):Bool;
	function new(bindable:Bool):Void;
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
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
