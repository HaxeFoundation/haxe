package cs.system;

/** Converts a base data type to another base data type. */
@:native("System.Convert")
extern class Convert {
	/** A constant that represents a database column that is absent of data; that is, database null. */
	static var DBNull(default, never):Dynamic;
	@:overload(function(value:Dynamic, conversionType:cs.system.Type):Dynamic {})
	@:overload(function(value:Dynamic, typeCode:cs.system.TypeCode):Dynamic {})
	@:overload(function(value:Dynamic, conversionType:cs.system.Type, provider:cs.system.IFormatProvider):Dynamic {})
	/**
	 * Returns an object of the specified type and whose value is equivalent to the
	 * specified object.
	 * @param value An object that implements the  interface.
	 * @param conversionType The type of object to return.
	 * @return An object whose type is  and whose value is equivalent to . -or- A null
	 * reference ( in Visual Basic), if  is  and  is not a value type.
	 */
	static function ChangeType(value:Dynamic, typeCode:cs.system.TypeCode, provider:cs.system.IFormatProvider):Dynamic;
	/**
	 * Converts a subset of a Unicode character array, which encodes binary data as
	 * base-64 digits, to an equivalent 8-bit unsigned integer array. Parameters
	 * specify the subset in the input array and the number of elements to convert.
	 * @param inArray A Unicode character array.
	 * @param offset A position within .
	 * @param length The number of elements in  to convert.
	 * @return An array of 8-bit unsigned integers equivalent to  elements at position 
	 * in .
	 */
	static function FromBase64CharArray(inArray:cs.NativeArray<cs.Char16>, offset:Int, length:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Converts the specified string, which encodes binary data as base-64 digits, to
	 * an equivalent 8-bit unsigned integer array.
	 * @param s The string to convert.
	 * @return An array of 8-bit unsigned integers that is equivalent to .
	 */
	static function FromBase64String(s:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the  for the specified object.
	 * @param value An object that implements the  interface.
	 * @return The  for , or  if  is .
	 */
	static function GetTypeCode(value:Dynamic):cs.system.TypeCode;
	/**
	 * Returns an indication whether the specified object is of type .
	 * @param value An object.
	 * @return if  is of type ; otherwise, .
	 */
	static function IsDBNull(value:Dynamic):Bool;
	@:overload(function(inArray:cs.NativeArray<cs.UInt8>, offsetIn:Int, length:Int, outArray:cs.NativeArray<cs.Char16>, offsetOut:Int):Int {})
	/**
	 * Converts a subset of an 8-bit unsigned integer array to an equivalent subset of
	 * a Unicode character array encoded with base-64 digits. Parameters specify the
	 * subsets as offsets in the input and output arrays, and the number of elements in
	 * the input array to convert.
	 * @param inArray An input array of 8-bit unsigned integers.
	 * @param offsetIn A position within .
	 * @param length The number of elements of  to convert.
	 * @param outArray An output array of Unicode characters.
	 * @param offsetOut A position within .
	 * @return A 32-bit signed integer containing the number of bytes in .
	 */
	static function ToBase64CharArray(inArray:cs.NativeArray<cs.UInt8>, offsetIn:Int, length:Int, outArray:cs.NativeArray<cs.Char16>, offsetOut:Int, options:cs.system.Base64FormattingOptions):Int;
	@:overload(function(inArray:cs.NativeArray<cs.UInt8>):String {})
	@:overload(function(inArray:cs.NativeArray<cs.UInt8>, options:cs.system.Base64FormattingOptions):String {})
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>, ?options:cs.system.Base64FormattingOptions):String {})
	@:overload(function(inArray:cs.NativeArray<cs.UInt8>, offset:Int, length:Int):String {})
	/**
	 * Converts an array of 8-bit unsigned integers to its equivalent string
	 * representation that is encoded with base-64 digits.
	 * @param inArray An array of 8-bit unsigned integers.
	 * @return The string representation, in base 64, of the contents of .
	 */
	static function ToBase64String(inArray:cs.NativeArray<cs.UInt8>, offset:Int, length:Int, options:cs.system.Base64FormattingOptions):String;
	@:overload(function(value:Bool):Bool {})
	@:overload(function(value:cs.UInt8):Bool {})
	@:overload(function(value:cs.Char16):Bool {})
	@:overload(function(value:cs.system.DateTime):Bool {})
	@:overload(function(value:cs.system.Decimal):Bool {})
	@:overload(function(value:Float):Bool {})
	@:overload(function(value:cs.Int16):Bool {})
	@:overload(function(value:Int):Bool {})
	@:overload(function(value:haxe.Int64):Bool {})
	@:overload(function(value:Dynamic):Bool {})
	@:overload(function(value:cs.Int8):Bool {})
	@:overload(function(value:Single):Bool {})
	@:overload(function(value:String):Bool {})
	@:overload(function(value:cs.UInt16):Bool {})
	@:overload(function(value:cs.UInt):Bool {})
	@:overload(function(value:cs.UInt64):Bool {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):Bool {})
	/**
	 * Returns the specified Boolean value; no actual conversion is performed.
	 * @param value The Boolean value to return.
	 * @return is returned unchanged.
	 */
	static function ToBoolean(value:String, provider:cs.system.IFormatProvider):Bool;
	@:overload(function(value:Bool):cs.UInt8 {})
	@:overload(function(value:cs.UInt8):cs.UInt8 {})
	@:overload(function(value:cs.Char16):cs.UInt8 {})
	@:overload(function(value:cs.system.DateTime):cs.UInt8 {})
	@:overload(function(value:cs.system.Decimal):cs.UInt8 {})
	@:overload(function(value:Float):cs.UInt8 {})
	@:overload(function(value:cs.Int16):cs.UInt8 {})
	@:overload(function(value:Int):cs.UInt8 {})
	@:overload(function(value:haxe.Int64):cs.UInt8 {})
	@:overload(function(value:Dynamic):cs.UInt8 {})
	@:overload(function(value:cs.Int8):cs.UInt8 {})
	@:overload(function(value:Single):cs.UInt8 {})
	@:overload(function(value:String):cs.UInt8 {})
	@:overload(function(value:cs.UInt16):cs.UInt8 {})
	@:overload(function(value:cs.UInt):cs.UInt8 {})
	@:overload(function(value:cs.UInt64):cs.UInt8 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.UInt8 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.UInt8 {})
	/**
	 * Converts the specified Boolean value to the equivalent 8-bit unsigned integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToByte(value:String, fromBase:Int):cs.UInt8;
	@:overload(function(value:Bool):cs.Char16 {})
	@:overload(function(value:cs.UInt8):cs.Char16 {})
	@:overload(function(value:cs.Char16):cs.Char16 {})
	@:overload(function(value:cs.system.DateTime):cs.Char16 {})
	@:overload(function(value:cs.system.Decimal):cs.Char16 {})
	@:overload(function(value:Float):cs.Char16 {})
	@:overload(function(value:cs.Int16):cs.Char16 {})
	@:overload(function(value:Int):cs.Char16 {})
	@:overload(function(value:haxe.Int64):cs.Char16 {})
	@:overload(function(value:Dynamic):cs.Char16 {})
	@:overload(function(value:cs.Int8):cs.Char16 {})
	@:overload(function(value:Single):cs.Char16 {})
	@:overload(function(value:String):cs.Char16 {})
	@:overload(function(value:cs.UInt16):cs.Char16 {})
	@:overload(function(value:cs.UInt):cs.Char16 {})
	@:overload(function(value:cs.UInt64):cs.Char16 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.Char16 {})
	/**
	 * Calling this method always throws .
	 * @param value The Boolean value to convert.
	 * @return This conversion is not supported. No value is returned.
	 */
	static function ToChar(value:String, provider:cs.system.IFormatProvider):cs.Char16;
	@:overload(function(value:Bool):cs.system.DateTime {})
	@:overload(function(value:cs.UInt8):cs.system.DateTime {})
	@:overload(function(value:cs.Char16):cs.system.DateTime {})
	@:overload(function(value:cs.system.DateTime):cs.system.DateTime {})
	@:overload(function(value:cs.system.Decimal):cs.system.DateTime {})
	@:overload(function(value:Float):cs.system.DateTime {})
	@:overload(function(value:cs.Int16):cs.system.DateTime {})
	@:overload(function(value:Int):cs.system.DateTime {})
	@:overload(function(value:haxe.Int64):cs.system.DateTime {})
	@:overload(function(value:Dynamic):cs.system.DateTime {})
	@:overload(function(value:cs.Int8):cs.system.DateTime {})
	@:overload(function(value:Single):cs.system.DateTime {})
	@:overload(function(value:String):cs.system.DateTime {})
	@:overload(function(value:cs.UInt16):cs.system.DateTime {})
	@:overload(function(value:cs.UInt):cs.system.DateTime {})
	@:overload(function(value:cs.UInt64):cs.system.DateTime {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.system.DateTime {})
	/**
	 * Calling this method always throws .
	 * @param value The Boolean value to convert.
	 * @return This conversion is not supported. No value is returned.
	 */
	static function ToDateTime(value:String, provider:cs.system.IFormatProvider):cs.system.DateTime;
	@:overload(function(value:Bool):cs.system.Decimal {})
	@:overload(function(value:cs.UInt8):cs.system.Decimal {})
	@:overload(function(value:cs.Char16):cs.system.Decimal {})
	@:overload(function(value:cs.system.DateTime):cs.system.Decimal {})
	@:overload(function(value:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(value:Float):cs.system.Decimal {})
	@:overload(function(value:cs.Int16):cs.system.Decimal {})
	@:overload(function(value:Int):cs.system.Decimal {})
	@:overload(function(value:haxe.Int64):cs.system.Decimal {})
	@:overload(function(value:Dynamic):cs.system.Decimal {})
	@:overload(function(value:cs.Int8):cs.system.Decimal {})
	@:overload(function(value:Single):cs.system.Decimal {})
	@:overload(function(value:String):cs.system.Decimal {})
	@:overload(function(value:cs.UInt16):cs.system.Decimal {})
	@:overload(function(value:cs.UInt):cs.system.Decimal {})
	@:overload(function(value:cs.UInt64):cs.system.Decimal {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.system.Decimal {})
	/**
	 * Converts the specified Boolean value to the equivalent decimal number.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToDecimal(value:String, provider:cs.system.IFormatProvider):cs.system.Decimal;
	@:overload(function(value:Bool):Float {})
	@:overload(function(value:cs.UInt8):Float {})
	@:overload(function(value:cs.Char16):Float {})
	@:overload(function(value:cs.system.DateTime):Float {})
	@:overload(function(value:cs.system.Decimal):Float {})
	@:overload(function(value:Float):Float {})
	@:overload(function(value:cs.Int16):Float {})
	@:overload(function(value:Int):Float {})
	@:overload(function(value:haxe.Int64):Float {})
	@:overload(function(value:Dynamic):Float {})
	@:overload(function(value:cs.Int8):Float {})
	@:overload(function(value:Single):Float {})
	@:overload(function(value:String):Float {})
	@:overload(function(value:cs.UInt16):Float {})
	@:overload(function(value:cs.UInt):Float {})
	@:overload(function(value:cs.UInt64):Float {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):Float {})
	/**
	 * Converts the specified Boolean value to the equivalent double-precision
	 * floating-point number.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToDouble(value:String, provider:cs.system.IFormatProvider):Float;
	@:overload(function(value:Bool):cs.Int16 {})
	@:overload(function(value:cs.UInt8):cs.Int16 {})
	@:overload(function(value:cs.Char16):cs.Int16 {})
	@:overload(function(value:cs.system.DateTime):cs.Int16 {})
	@:overload(function(value:cs.system.Decimal):cs.Int16 {})
	@:overload(function(value:Float):cs.Int16 {})
	@:overload(function(value:cs.Int16):cs.Int16 {})
	@:overload(function(value:Int):cs.Int16 {})
	@:overload(function(value:haxe.Int64):cs.Int16 {})
	@:overload(function(value:Dynamic):cs.Int16 {})
	@:overload(function(value:cs.Int8):cs.Int16 {})
	@:overload(function(value:Single):cs.Int16 {})
	@:overload(function(value:String):cs.Int16 {})
	@:overload(function(value:cs.UInt16):cs.Int16 {})
	@:overload(function(value:cs.UInt):cs.Int16 {})
	@:overload(function(value:cs.UInt64):cs.Int16 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.Int16 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.Int16 {})
	/**
	 * Converts the specified Boolean value to the equivalent 16-bit signed integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToInt16(value:String, fromBase:Int):cs.Int16;
	@:overload(function(value:Bool):Int {})
	@:overload(function(value:cs.UInt8):Int {})
	@:overload(function(value:cs.Char16):Int {})
	@:overload(function(value:cs.system.DateTime):Int {})
	@:overload(function(value:cs.system.Decimal):Int {})
	@:overload(function(value:Float):Int {})
	@:overload(function(value:cs.Int16):Int {})
	@:overload(function(value:Int):Int {})
	@:overload(function(value:haxe.Int64):Int {})
	@:overload(function(value:Dynamic):Int {})
	@:overload(function(value:cs.Int8):Int {})
	@:overload(function(value:Single):Int {})
	@:overload(function(value:String):Int {})
	@:overload(function(value:cs.UInt16):Int {})
	@:overload(function(value:cs.UInt):Int {})
	@:overload(function(value:cs.UInt64):Int {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):Int {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):Int {})
	/**
	 * Converts the specified Boolean value to the equivalent 32-bit signed integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToInt32(value:String, fromBase:Int):Int;
	@:overload(function(value:Bool):haxe.Int64 {})
	@:overload(function(value:cs.UInt8):haxe.Int64 {})
	@:overload(function(value:cs.Char16):haxe.Int64 {})
	@:overload(function(value:cs.system.DateTime):haxe.Int64 {})
	@:overload(function(value:cs.system.Decimal):haxe.Int64 {})
	@:overload(function(value:Float):haxe.Int64 {})
	@:overload(function(value:cs.Int16):haxe.Int64 {})
	@:overload(function(value:Int):haxe.Int64 {})
	@:overload(function(value:haxe.Int64):haxe.Int64 {})
	@:overload(function(value:Dynamic):haxe.Int64 {})
	@:overload(function(value:cs.Int8):haxe.Int64 {})
	@:overload(function(value:Single):haxe.Int64 {})
	@:overload(function(value:String):haxe.Int64 {})
	@:overload(function(value:cs.UInt16):haxe.Int64 {})
	@:overload(function(value:cs.UInt):haxe.Int64 {})
	@:overload(function(value:cs.UInt64):haxe.Int64 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):haxe.Int64 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):haxe.Int64 {})
	/**
	 * Converts the specified Boolean value to the equivalent 64-bit signed integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToInt64(value:String, fromBase:Int):haxe.Int64;
	@:overload(function(value:Bool):cs.Int8 {})
	@:overload(function(value:cs.UInt8):cs.Int8 {})
	@:overload(function(value:cs.Char16):cs.Int8 {})
	@:overload(function(value:cs.system.DateTime):cs.Int8 {})
	@:overload(function(value:cs.system.Decimal):cs.Int8 {})
	@:overload(function(value:Float):cs.Int8 {})
	@:overload(function(value:cs.Int16):cs.Int8 {})
	@:overload(function(value:Int):cs.Int8 {})
	@:overload(function(value:haxe.Int64):cs.Int8 {})
	@:overload(function(value:Dynamic):cs.Int8 {})
	@:overload(function(value:cs.Int8):cs.Int8 {})
	@:overload(function(value:Single):cs.Int8 {})
	@:overload(function(value:String):cs.Int8 {})
	@:overload(function(value:cs.UInt16):cs.Int8 {})
	@:overload(function(value:cs.UInt):cs.Int8 {})
	@:overload(function(value:cs.UInt64):cs.Int8 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.Int8 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.Int8 {})
	/**
	 * Converts the specified Boolean value to the equivalent 8-bit signed integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToSByte(value:String, fromBase:Int):cs.Int8;
	@:overload(function(value:Bool):Single {})
	@:overload(function(value:cs.UInt8):Single {})
	@:overload(function(value:cs.Char16):Single {})
	@:overload(function(value:cs.system.DateTime):Single {})
	@:overload(function(value:cs.system.Decimal):Single {})
	@:overload(function(value:Float):Single {})
	@:overload(function(value:cs.Int16):Single {})
	@:overload(function(value:Int):Single {})
	@:overload(function(value:haxe.Int64):Single {})
	@:overload(function(value:Dynamic):Single {})
	@:overload(function(value:cs.Int8):Single {})
	@:overload(function(value:Single):Single {})
	@:overload(function(value:String):Single {})
	@:overload(function(value:cs.UInt16):Single {})
	@:overload(function(value:cs.UInt):Single {})
	@:overload(function(value:cs.UInt64):Single {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):Single {})
	/**
	 * Converts the specified Boolean value to the equivalent single-precision
	 * floating-point number.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToSingle(value:String, provider:cs.system.IFormatProvider):Single;
	@:overload(function(value:Bool):String {})
	@:overload(function(value:cs.UInt8):String {})
	@:overload(function(value:cs.Char16):String {})
	@:overload(function(value:cs.system.DateTime):String {})
	@:overload(function(value:cs.system.Decimal):String {})
	@:overload(function(value:Float):String {})
	@:overload(function(value:cs.Int16):String {})
	@:overload(function(value:Int):String {})
	@:overload(function(value:haxe.Int64):String {})
	@:overload(function(value:Dynamic):String {})
	@:overload(function(value:cs.Int8):String {})
	@:overload(function(value:Single):String {})
	@:overload(function(value:String):String {})
	@:overload(function(value:cs.UInt16):String {})
	@:overload(function(value:cs.UInt):String {})
	@:overload(function(value:cs.UInt64):String {})
	@:overload(function(value:Bool, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.UInt8, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.UInt8, toBase:Int):String {})
	@:overload(function(value:cs.Char16, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.system.DateTime, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.system.Decimal, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:Float, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.Int16, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.Int16, toBase:Int):String {})
	@:overload(function(value:Int, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:Int, toBase:Int):String {})
	@:overload(function(value:haxe.Int64, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:haxe.Int64, toBase:Int):String {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.Int8, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:Single, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.UInt16, provider:cs.system.IFormatProvider):String {})
	@:overload(function(value:cs.UInt, provider:cs.system.IFormatProvider):String {})
	/**
	 * Converts the specified Boolean value to its equivalent string representation.
	 * @param value The Boolean value to convert.
	 * @return The string representation of .
	 */
	static function ToString(value:cs.UInt64, provider:cs.system.IFormatProvider):String;
	@:overload(function(value:Bool):cs.UInt16 {})
	@:overload(function(value:cs.UInt8):cs.UInt16 {})
	@:overload(function(value:cs.Char16):cs.UInt16 {})
	@:overload(function(value:cs.system.DateTime):cs.UInt16 {})
	@:overload(function(value:cs.system.Decimal):cs.UInt16 {})
	@:overload(function(value:Float):cs.UInt16 {})
	@:overload(function(value:cs.Int16):cs.UInt16 {})
	@:overload(function(value:Int):cs.UInt16 {})
	@:overload(function(value:haxe.Int64):cs.UInt16 {})
	@:overload(function(value:Dynamic):cs.UInt16 {})
	@:overload(function(value:cs.Int8):cs.UInt16 {})
	@:overload(function(value:Single):cs.UInt16 {})
	@:overload(function(value:String):cs.UInt16 {})
	@:overload(function(value:cs.UInt16):cs.UInt16 {})
	@:overload(function(value:cs.UInt):cs.UInt16 {})
	@:overload(function(value:cs.UInt64):cs.UInt16 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.UInt16 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.UInt16 {})
	/**
	 * Converts the specified Boolean value to the equivalent 16-bit unsigned integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToUInt16(value:String, fromBase:Int):cs.UInt16;
	@:overload(function(value:Bool):cs.UInt {})
	@:overload(function(value:cs.UInt8):cs.UInt {})
	@:overload(function(value:cs.Char16):cs.UInt {})
	@:overload(function(value:cs.system.DateTime):cs.UInt {})
	@:overload(function(value:cs.system.Decimal):cs.UInt {})
	@:overload(function(value:Float):cs.UInt {})
	@:overload(function(value:cs.Int16):cs.UInt {})
	@:overload(function(value:Int):cs.UInt {})
	@:overload(function(value:haxe.Int64):cs.UInt {})
	@:overload(function(value:Dynamic):cs.UInt {})
	@:overload(function(value:cs.Int8):cs.UInt {})
	@:overload(function(value:Single):cs.UInt {})
	@:overload(function(value:String):cs.UInt {})
	@:overload(function(value:cs.UInt16):cs.UInt {})
	@:overload(function(value:cs.UInt):cs.UInt {})
	@:overload(function(value:cs.UInt64):cs.UInt {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.UInt {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.UInt {})
	/**
	 * Converts the specified Boolean value to the equivalent 32-bit unsigned integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToUInt32(value:String, fromBase:Int):cs.UInt;
	@:overload(function(value:Bool):cs.UInt64 {})
	@:overload(function(value:cs.UInt8):cs.UInt64 {})
	@:overload(function(value:cs.Char16):cs.UInt64 {})
	@:overload(function(value:cs.system.DateTime):cs.UInt64 {})
	@:overload(function(value:cs.system.Decimal):cs.UInt64 {})
	@:overload(function(value:Float):cs.UInt64 {})
	@:overload(function(value:cs.Int16):cs.UInt64 {})
	@:overload(function(value:Int):cs.UInt64 {})
	@:overload(function(value:haxe.Int64):cs.UInt64 {})
	@:overload(function(value:Dynamic):cs.UInt64 {})
	@:overload(function(value:cs.Int8):cs.UInt64 {})
	@:overload(function(value:Single):cs.UInt64 {})
	@:overload(function(value:String):cs.UInt64 {})
	@:overload(function(value:cs.UInt16):cs.UInt64 {})
	@:overload(function(value:cs.UInt):cs.UInt64 {})
	@:overload(function(value:cs.UInt64):cs.UInt64 {})
	@:overload(function(value:Dynamic, provider:cs.system.IFormatProvider):cs.UInt64 {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.UInt64 {})
	/**
	 * Converts the specified Boolean value to the equivalent 64-bit unsigned integer.
	 * @param value The Boolean value to convert.
	 * @return The number 1 if  is ; otherwise, 0.
	 */
	static function ToUInt64(value:String, fromBase:Int):cs.UInt64;
	/**
	 * @param chars 
	 * @param bytes 
	 * @param bytesWritten 
	 */
	static function TryFromBase64Chars(chars:cs.system.ReadOnlySpan<cs.Char16>, bytes:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param s 
	 * @param bytes 
	 * @param bytesWritten 
	 */
	static function TryFromBase64String(s:String, bytes:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param bytes 
	 * @param chars 
	 * @param charsWritten 
	 * @param options 
	 */
	static function TryToBase64Chars(bytes:cs.system.ReadOnlySpan<cs.UInt8>, chars:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?options:cs.system.Base64FormattingOptions):Bool;
}
