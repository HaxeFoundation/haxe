package cs.system.runtime.serialization;

/** Represents a base implementation of the  interface that uses the  class and the  interface. */
@:native("System.Runtime.Serialization.FormatterConverter")
extern class FormatterConverter {
	function new():Void;
	@:overload(function(value:Dynamic, type:cs.system.Type):Dynamic {})
	/**
	 * Converts a value to the given .
	 * @param value The object to convert.
	 * @param type The  into which  is converted.
	 * @return The converted  or  if the  parameter is .
	 */
	function Convert(value:Dynamic, typeCode:cs.system.TypeCode):Dynamic;
	/**
	 * Converts a value to a .
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToBoolean(value:Dynamic):Bool;
	/**
	 * Converts a value to an 8-bit unsigned integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToByte(value:Dynamic):cs.UInt8;
	/**
	 * Converts a value to a Unicode character.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToChar(value:Dynamic):cs.Char16;
	/**
	 * Converts a value to a .
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToDateTime(value:Dynamic):cs.system.DateTime;
	/**
	 * Converts a value to a .
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToDecimal(value:Dynamic):cs.system.Decimal;
	/**
	 * Converts a value to a double-precision floating-point number.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToDouble(value:Dynamic):Float;
	/**
	 * Converts a value to a 16-bit signed integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToInt16(value:Dynamic):cs.Int16;
	/**
	 * Converts a value to a 32-bit signed integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToInt32(value:Dynamic):Int;
	/**
	 * Converts a value to a 64-bit signed integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToInt64(value:Dynamic):haxe.Int64;
	/**
	 * Converts a value to a .
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToSByte(value:Dynamic):cs.Int8;
	/**
	 * Converts a value to a single-precision floating-point number.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToSingle(value:Dynamic):Single;
	/**
	 * Converts the specified object to a .
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToString(value:Dynamic):String;
	/**
	 * Converts a value to a 16-bit unsigned integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToUInt16(value:Dynamic):cs.UInt16;
	/**
	 * Converts a value to a 32-bit unsigned integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToUInt32(value:Dynamic):cs.UInt;
	/**
	 * Converts a value to a 64-bit unsigned integer.
	 * @param value The object to convert.
	 * @return The converted  or  if the  parameter is .
	 */
	function ToUInt64(value:Dynamic):cs.UInt64;
}
