package cs.system.componentmodel;

/** Indicates that an object's text representation is obscured by characters such as asterisks. This class cannot be inherited. */
@:native("System.ComponentModel.PasswordPropertyTextAttribute")
extern class PasswordPropertyTextAttribute extends cs.system.Attribute {
	/** Specifies the default value for the . */
	static var Default(default, never):cs.system.componentmodel.PasswordPropertyTextAttribute;
	/** Specifies that a text property is not used as a password. This  ( in Visual Basic) field is read-only. */
	static var No(default, never):cs.system.componentmodel.PasswordPropertyTextAttribute;
	/** Specifies that a text property is used as a password. This  ( in Visual Basic) field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.PasswordPropertyTextAttribute;
	/**
	 * Gets a value indicating if the property for which the  is defined should be
	 * shown as password text.
	 * @return if the property should be shown as password text; otherwise, .
	 */
	var Password(default, never):Bool;
	@:overload(function():Void {})
	function new(password:Bool):Void;
	/**
	 * Determines whether two  instances are equal.
	 * @param o The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns an indication whether the value of this instance is the default value.
	 * @return if this instance is the default attribute for the class; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
