package cs.system.componentmodel;

/** Specifies a description for a property or event. */
@:native("System.ComponentModel.DescriptionAttribute")
extern class DescriptionAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is an empty string (""). This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DescriptionAttribute;
	/**
	 * Gets the description stored in this attribute.
	 * @return The description stored in this attribute.
	 */
	var Description(default, never):String;
	/**
	 * Gets or sets the string stored as the description.
	 * @return The string stored as the description. The default value is an empty
	 * string ("").
	 */
	var DescriptionValue(default, default):String;
	@:overload(function():Void {})
	function new(description:String):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a value indicating whether this is the default  instance.
	 * @return , if this is the default  instance; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
