package cs.system;

/** Converts base data types to an array of bytes, and an array of bytes to base data types. */
@:native("System.BitConverter")
extern class BitConverter {
	/** Indicates the byte order ("endianness") in which data is stored in this computer architecture. */
	static var IsLittleEndian(default, never):Bool;
	/**
	 * Converts the specified double-precision floating point number to a 64-bit signed
	 * integer.
	 * @param value The number to convert.
	 * @return A 64-bit signed integer whose value is equivalent to .
	 */
	static function DoubleToInt64Bits(value:Float):haxe.Int64;
	@:overload(function(value:Bool):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:cs.Char16):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:Float):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:cs.Int16):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:Int):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:haxe.Int64):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:Single):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:cs.UInt16):cs.NativeArray<cs.UInt8> {})
	@:overload(function(value:cs.UInt):cs.NativeArray<cs.UInt8> {})
	/**
	 * Returns the specified Boolean value as a byte array.
	 * @param value A Boolean value.
	 * @return A byte array with length 1.
	 */
	static function GetBytes(value:cs.UInt64):cs.NativeArray<cs.UInt8>;
	/** @param value  */
	static function Int32BitsToSingle(value:Int):Single;
	/**
	 * Converts the specified 64-bit signed integer to a double-precision floating
	 * point number.
	 * @param value The number to convert.
	 * @return A double-precision floating point number whose value is equivalent to .
	 */
	static function Int64BitsToDouble(value:haxe.Int64):Float;
	/** @param value  */
	static function SingleToInt32Bits(value:Single):Int;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):Bool {})
	/**
	 * Returns a Boolean value converted from the byte at a specified position in a
	 * byte array.
	 * @param value A byte array.
	 * @param startIndex The index of the byte within .
	 * @return if the byte at  in  is nonzero; otherwise, .
	 */
	static function ToBoolean(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Bool;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):cs.Char16 {})
	/**
	 * Returns a Unicode character converted from two bytes at a specified position in
	 * a byte array.
	 * @param value An array.
	 * @param startIndex The starting position within .
	 * @return A character formed by two bytes beginning at .
	 */
	static function ToChar(value:cs.NativeArray<cs.UInt8>, startIndex:Int):cs.Char16;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):Float {})
	/**
	 * Returns a double-precision floating point number converted from eight bytes at a
	 * specified position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A double precision floating point number formed by eight bytes beginning
	 * at .
	 */
	static function ToDouble(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Float;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):cs.Int16 {})
	/**
	 * Returns a 16-bit signed integer converted from two bytes at a specified position
	 * in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 16-bit signed integer formed by two bytes beginning at .
	 */
	static function ToInt16(value:cs.NativeArray<cs.UInt8>, startIndex:Int):cs.Int16;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):Int {})
	/**
	 * Returns a 32-bit signed integer converted from four bytes at a specified
	 * position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 32-bit signed integer formed by four bytes beginning at .
	 */
	static function ToInt32(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Int;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):haxe.Int64 {})
	/**
	 * Returns a 64-bit signed integer converted from eight bytes at a specified
	 * position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 64-bit signed integer formed by eight bytes beginning at .
	 */
	static function ToInt64(value:cs.NativeArray<cs.UInt8>, startIndex:Int):haxe.Int64;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):Single {})
	/**
	 * Returns a single-precision floating point number converted from four bytes at a
	 * specified position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A single-precision floating point number formed by four bytes beginning
	 * at .
	 */
	static function ToSingle(value:cs.NativeArray<cs.UInt8>, startIndex:Int):Single;
	@:overload(function(value:cs.NativeArray<cs.UInt8>):String {})
	@:overload(function(value:cs.NativeArray<cs.UInt8>, startIndex:Int):String {})
	/**
	 * Converts the numeric value of each element of a specified array of bytes to its
	 * equivalent hexadecimal string representation.
	 * @param value An array of bytes.
	 * @return A string of hexadecimal pairs separated by hyphens, where each pair
	 * represents the corresponding element in ; for example, "7F-2C-4A-00".
	 */
	static function ToString(value:cs.NativeArray<cs.UInt8>, startIndex:Int, length:Int):String;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt16 {})
	/**
	 * Returns a 16-bit unsigned integer converted from two bytes at a specified
	 * position in a byte array.
	 * @param value The array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 16-bit unsigned integer formed by two bytes beginning at .
	 */
	static function ToUInt16(value:cs.NativeArray<cs.UInt8>, startIndex:Int):cs.UInt16;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt {})
	/**
	 * Returns a 32-bit unsigned integer converted from four bytes at a specified
	 * position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 32-bit unsigned integer formed by four bytes beginning at .
	 */
	static function ToUInt32(value:cs.NativeArray<cs.UInt8>, startIndex:Int):cs.UInt;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt64 {})
	/**
	 * Returns a 64-bit unsigned integer converted from eight bytes at a specified
	 * position in a byte array.
	 * @param value An array of bytes.
	 * @param startIndex The starting position within .
	 * @return A 64-bit unsigned integer formed by the eight bytes beginning at .
	 */
	static function ToUInt64(value:cs.NativeArray<cs.UInt8>, startIndex:Int):cs.UInt64;
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:Bool):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:cs.Char16):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:Float):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:cs.Int16):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:Int):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:haxe.Int64):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:Single):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:cs.UInt16):Bool {})
	@:overload(function(destination:cs.system.Span<cs.UInt8>, value:cs.UInt):Bool {})
	/**
	 * @param destination 
	 * @param value 
	 */
	static function TryWriteBytes(destination:cs.system.Span<cs.UInt8>, value:cs.UInt64):Bool;
}
