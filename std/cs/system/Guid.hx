package cs.system;

/** Represents a globally unique identifier (GUID). */
@:native("System.Guid")
extern class Guid extends cs.system.ValueType {
	/** A read-only instance of the  structure whose value is all zeros. */
	static var Empty(default, never):cs.system.Guid;
	@:overload(function(b:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(b:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	@:overload(function(g:String):Void {})
	@:overload(function(a:Int, b:cs.Int16, c:cs.Int16, d:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(a:Int, b:cs.Int16, c:cs.Int16, d:cs.UInt8, e:cs.UInt8, f:cs.UInt8, g:cs.UInt8, h:cs.UInt8, i:cs.UInt8, j:cs.UInt8, k:cs.UInt8):Void {})
	function new(a:cs.UInt, b:cs.UInt16, c:cs.UInt16, d:cs.UInt8, e:cs.UInt8, f:cs.UInt8, g:cs.UInt8, h:cs.UInt8, i:cs.UInt8, j:cs.UInt8, k:cs.UInt8):Void;
	/**
	 * Initializes a new instance of the  structure.
	 * @return A new GUID object.
	 */
	static function NewGuid():cs.system.Guid;
	/**
	 * Indicates whether the values of two specified  objects are equal.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(a:cs.system.Guid, b:cs.system.Guid):Bool;
	/**
	 * Indicates whether the values of two specified  objects are not equal.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(a:cs.system.Guid, b:cs.system.Guid):Bool;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>):cs.system.Guid {})
	/** @param input  */
	static function Parse(input:String):cs.system.Guid;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>):cs.system.Guid {})
	/**
	 * @param input 
	 * @param format 
	 */
	static function ParseExact(input:String, format:String):cs.system.Guid;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.Guid>):Bool {})
	/**
	 * @param input 
	 * @param result 
	 */
	static function TryParse(input:String, result:cs.Ref<cs.system.Guid>):Bool;
	@:overload(function(input:cs.system.ReadOnlySpan<cs.Char16>, format:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.Guid>):Bool {})
	/**
	 * @param input 
	 * @param format 
	 * @param result 
	 */
	static function TryParseExact(input:String, format:String, result:cs.Ref<cs.system.Guid>):Bool;
	@:overload(function(value:cs.system.Guid):Int {})
	/**
	 * Compares this instance to a specified  object and returns an indication of their
	 * relative values.
	 * @param value An object to compare to this instance.
	 * @return A signed number indicating the relative values of this instance and .
	 * Return value Description A negative integer This instance is less than . Zero
	 * This instance is equal to . A positive integer This instance is greater than .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(g:cs.system.Guid):Bool {})
	/**
	 * Returns a value indicating whether this instance and a specified  object
	 * represent the same value.
	 * @param g An object to compare to this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a 16-element byte array that contains the value of this instance.
	 * @return A 16-element byte array.
	 */
	function ToByteArray():cs.NativeArray<cs.UInt8>;
	@:overload(function():String {})
	@:overload(function(format:String):String {})
	/**
	 * Returns a string representation of the value of this instance in registry
	 * format.
	 * @return The value of this , formatted by using the "D" format specifier as
	 * follows: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx where the value of the GUID is
	 * represented as a series of lowercase hexadecimal digits in groups of 8, 4, 4, 4,
	 * and 12 digits and separated by hyphens. An example of a return value is
	 * "382c74c3-721d-4f34-80e5-57657b6cbc27". To convert the hexadecimal digits from a
	 * through f to uppercase, call the  method on the returned string.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>):Bool;
	/** @param destination  */
	function TryWriteBytes(destination:cs.system.Span<cs.UInt8>):Bool;
}
