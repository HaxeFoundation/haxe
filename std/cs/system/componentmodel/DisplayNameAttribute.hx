package cs.system.componentmodel;

/** Specifies the display name for a property, event, or public void method which takes no arguments. */
@:native("System.ComponentModel.DisplayNameAttribute")
extern class DisplayNameAttribute extends cs.system.Attribute {
	/** Specifies the default value for the . This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DisplayNameAttribute;
	/**
	 * Gets the display name for a property, event, or public void method that takes no
	 * arguments stored in this attribute.
	 * @return The display name.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets or sets the display name.
	 * @return The display name.
	 */
	var DisplayNameValue(default, default):String;
	@:overload(function():Void {})
	function new(displayName:String):Void;
	/**
	 * Determines whether two  instances are equal.
	 * @param obj The  to test the value equality of.
	 * @return if the value of the given object is equal to that of the current object;
	 * otherwise, .
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
