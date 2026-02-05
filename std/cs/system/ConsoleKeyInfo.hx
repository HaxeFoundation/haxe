package cs.system;

/** Describes the console key that was pressed, including the character represented by the console key and the state of the SHIFT, ALT, and CTRL modifier keys. */
@:native("System.ConsoleKeyInfo")
extern class ConsoleKeyInfo extends cs.system.ValueType {
	/**
	 * Gets the console key represented by the current  object.
	 * @return A value that identifies the console key that was pressed.
	 */
	var Key(default, never):cs.system.ConsoleKey;
	/**
	 * Gets the Unicode character represented by the current  object.
	 * @return An object that corresponds to the console key represented by the current
	 * object.
	 */
	var KeyChar(default, never):cs.Char16;
	/**
	 * Gets a bitwise combination of  values that specifies one or more modifier keys
	 * pressed simultaneously with the console key.
	 * @return A bitwise combination of the enumeration values. There is no default
	 * value.
	 */
	var Modifiers(default, never):cs.system.ConsoleModifiers;
	function new(keyChar:cs.Char16, key:cs.system.ConsoleKey, shift:Bool, alt:Bool, control:Bool):Void;
	/**
	 * Indicates whether the specified  objects are equal.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(a:cs.system.ConsoleKeyInfo, b:cs.system.ConsoleKeyInfo):Bool;
	/**
	 * Indicates whether the specified  objects are not equal.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(a:cs.system.ConsoleKeyInfo, b:cs.system.ConsoleKeyInfo):Bool;
	@:overload(function(obj:cs.system.ConsoleKeyInfo):Bool {})
	/**
	 * Gets a value indicating whether the specified  object is equal to the current 
	 * object.
	 * @param obj An object to compare to the current  object.
	 * @return if  is equal to the current  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
