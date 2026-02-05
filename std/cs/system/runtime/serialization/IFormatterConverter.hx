package cs.system.runtime.serialization;

/** Provides the connection between an instance of  and the formatter-provided class best suited to parse the data inside the . */
@:native("System.Runtime.Serialization.IFormatterConverter")
extern interface IFormatterConverter {
	@:overload(function(value:Dynamic, type:cs.system.Type):Dynamic {})
	/**
	 * Converts a value to the given .
	 * @param value The object to be converted.
	 * @param type The  into which  is to be converted.
	 * @return The converted .
	 */
	function Convert(value:Dynamic, typeCode:cs.system.TypeCode):Dynamic;
	/**
	 * Converts a value to a .
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToBoolean(value:Dynamic):Bool;
	/**
	 * Converts a value to an 8-bit unsigned integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToByte(value:Dynamic):cs.UInt8;
	/**
	 * Converts a value to a Unicode character.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToChar(value:Dynamic):cs.Char16;
	/**
	 * Converts a value to a .
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToDateTime(value:Dynamic):cs.system.DateTime;
	/**
	 * Converts a value to a .
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToDecimal(value:Dynamic):cs.system.Decimal;
	/**
	 * Converts a value to a double-precision floating-point number.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToDouble(value:Dynamic):Float;
	/**
	 * Converts a value to a 16-bit signed integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToInt16(value:Dynamic):cs.Int16;
	/**
	 * Converts a value to a 32-bit signed integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToInt32(value:Dynamic):Int;
	/**
	 * Converts a value to a 64-bit signed integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToInt64(value:Dynamic):haxe.Int64;
	/**
	 * Converts a value to a .
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToSByte(value:Dynamic):cs.Int8;
	/**
	 * Converts a value to a single-precision floating-point number.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToSingle(value:Dynamic):Single;
	/**
	 * Converts a value to a .
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToString(value:Dynamic):String;
	/**
	 * Converts a value to a 16-bit unsigned integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToUInt16(value:Dynamic):cs.UInt16;
	/**
	 * Converts a value to a 32-bit unsigned integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToUInt32(value:Dynamic):cs.UInt;
	/**
	 * Converts a value to a 64-bit unsigned integer.
	 * @param value The object to be converted.
	 * @return The converted .
	 */
	function ToUInt64(value:Dynamic):cs.UInt64;
}
