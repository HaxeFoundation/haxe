package cs.system.componentmodel;

/** Specifies the value to pass to a property to cause the property to get its value from another source. This is known as ambience. This class cannot be inherited. */
@:native("System.ComponentModel.AmbientValueAttribute")
extern class AmbientValueAttribute extends cs.system.Attribute {
	/**
	 * Gets the object that is the value of this .
	 * @return The object that is the value of this .
	 */
	var Value(default, never):Dynamic;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.UInt8):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:cs.Int16):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	function new(type:cs.system.Type, value:String):Void;
	/**
	 * Determines whether the specified  is equal to the current .
	 * @param obj The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
