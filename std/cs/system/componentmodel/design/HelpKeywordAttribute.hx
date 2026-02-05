package cs.system.componentmodel.design;

/** Specifies the context keyword for a class or member. This class cannot be inherited. */
@:native("System.ComponentModel.Design.HelpKeywordAttribute")
extern class HelpKeywordAttribute extends cs.system.Attribute {
	/** Represents the default value for . This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.design.HelpKeywordAttribute;
	/**
	 * Gets the Help keyword supplied by this attribute.
	 * @return The Help keyword supplied by this attribute.
	 */
	var HelpKeyword(default, never):String;
	@:overload(function():Void {})
	@:overload(function(keyword:String):Void {})
	function new(t:cs.system.Type):Void;
	/**
	 * Determines whether two  instances are equal.
	 * @param obj The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Determines whether the Help keyword is .
	 * @return if the Help keyword is ; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
