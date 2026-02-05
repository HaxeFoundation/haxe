package cs.system.componentmodel;

/** Specifies whether a property should be localized. This class cannot be inherited. */
@:native("System.ComponentModel.LocalizableAttribute")
extern class LocalizableAttribute extends cs.system.Attribute {
	/** Specifies the default value, which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.LocalizableAttribute;
	/** Specifies that a property should not be localized. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.LocalizableAttribute;
	/** Specifies that a property should be localized. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.LocalizableAttribute;
	/**
	 * Gets a value indicating whether a property should be localized.
	 * @return if a property should be localized; otherwise, .
	 */
	var IsLocalizable(default, never):Bool;
	function new(isLocalizable:Bool):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
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
