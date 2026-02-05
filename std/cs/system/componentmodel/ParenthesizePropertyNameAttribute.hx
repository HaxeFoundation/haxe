package cs.system.componentmodel;

/** Indicates whether the name of the associated property is displayed with parentheses in the Properties window. This class cannot be inherited. */
@:native("System.ComponentModel.ParenthesizePropertyNameAttribute")
extern class ParenthesizePropertyNameAttribute extends cs.system.Attribute {
	/** Initializes a new instance of the  class with a default value that indicates that the associated property should not be shown with parentheses. This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.ParenthesizePropertyNameAttribute;
	/**
	 * Gets a value indicating whether the Properties window displays the name of the
	 * property in parentheses in the Properties window.
	 * @return if the property is displayed with parentheses; otherwise, .
	 */
	var NeedParenthesis(default, never):Bool;
	@:overload(function():Void {})
	function new(needParenthesis:Bool):Void;
	/**
	 * Compares the specified object to this object and tests for equality.
	 * @param o The object to be compared.
	 * @return if equal; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
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
