package cs.system.componentmodel;

/** Specifies the default value for a property. */
@:native("System.ComponentModel.DefaultValueAttribute")
extern class DefaultValueAttribute extends cs.system.Attribute {
	/**
	 * Gets the default value of the property this attribute is bound to.
	 * @return An  that represents the default value of the property this attribute is
	 * bound to.
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
	@:overload(function(value:cs.Int8):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt16):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	function new(type:cs.system.Type, value:String):Void;
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
}
