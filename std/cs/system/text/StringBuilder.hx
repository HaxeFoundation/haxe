package cs.system.text;

/** Represents a mutable string of characters. This class cannot be inherited. */
@:native("System.Text.StringBuilder")
extern class StringBuilder {
	/**
	 * Gets or sets the maximum number of characters that can be contained in the
	 * memory allocated by the current instance.
	 * @return The maximum number of characters that can be contained in the memory
	 * allocated by the current instance. Its value can range from  to .
	 */
	var Capacity(default, default):Int;
	var Chars(default, default):cs.Char16;
	/**
	 * Gets or sets the length of the current  object.
	 * @return The length of this instance.
	 */
	var Length(default, default):Int;
	/**
	 * Gets the maximum capacity of this instance.
	 * @return The maximum number of characters this instance can hold.
	 */
	var MaxCapacity(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(capacity:Int, maxCapacity:Int):Void {})
	@:overload(function(value:String, capacity:Int):Void {})
	function new(value:String, startIndex:Int, length:Int, capacity:Int):Void;
	@:overload(function(value:Bool):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.UInt8):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.Char16):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.NativeArray<cs.Char16>):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.system.Decimal):cs.system.text.StringBuilder {})
	@:overload(function(value:Float):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.Int16):cs.system.text.StringBuilder {})
	@:overload(function(value:Int):cs.system.text.StringBuilder {})
	@:overload(function(value:haxe.Int64):cs.system.text.StringBuilder {})
	@:overload(function(value:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.system.ReadOnlySpan<cs.Char16>):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.Int8):cs.system.text.StringBuilder {})
	@:overload(function(value:Single):cs.system.text.StringBuilder {})
	@:overload(function(value:String):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.system.text.StringBuilder):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.UInt16):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.UInt):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.UInt64):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.Pointer<cs.Char16>, valueCount:Int):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.Char16, repeatCount:Int):cs.system.text.StringBuilder {})
	@:overload(function(value:cs.NativeArray<cs.Char16>, startIndex:Int, charCount:Int):cs.system.text.StringBuilder {})
	@:overload(function(value:String, startIndex:Int, count:Int):cs.system.text.StringBuilder {})
	/**
	 * Appends the string representation of a specified Boolean value to this instance.
	 * @param value The Boolean value to append.
	 * @return A reference to this instance after the append operation has completed.
	 */
	function Append(value:cs.system.text.StringBuilder, startIndex:Int, count:Int):cs.system.text.StringBuilder;
	@:overload(function(format:String, arg0:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(format:String, args:cs.NativeArray<Dynamic>):cs.system.text.StringBuilder {})
	@:overload(function(provider:cs.system.IFormatProvider, format:String, arg0:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(provider:cs.system.IFormatProvider, format:String, args:cs.NativeArray<Dynamic>):cs.system.text.StringBuilder {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(provider:cs.system.IFormatProvider, format:String, arg0:Dynamic, arg1:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):cs.system.text.StringBuilder {})
	/**
	 * Appends the string returned by processing a composite format string, which
	 * contains zero or more format items, to this instance. Each format item is
	 * replaced by the string representation of a single argument using a specified
	 * format provider.
	 * @param provider An object that supplies culture-specific formatting information.
	 * @param format A composite format string.
	 * @param arg0 The object to format.
	 * @return A reference to this instance after the append operation has completed.
	 * After the append operation, this instance contains any data that existed before
	 * the operation, suffixed by a copy of  in which any format specification is
	 * replaced by the string representation of .
	 */
	function AppendFormat(provider:cs.system.IFormatProvider, format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):cs.system.text.StringBuilder;
	@:overload(function(separator:cs.Char16, values:cs.NativeArray<Dynamic>):cs.system.text.StringBuilder {})
	@:overload(function(separator:cs.Char16, values:cs.NativeArray<String>):cs.system.text.StringBuilder {})
	@:overload(function(separator:String, values:cs.NativeArray<Dynamic>):cs.system.text.StringBuilder {})
	@:overload(function(separator:String, values:cs.NativeArray<String>):cs.system.text.StringBuilder {})
	@:overload(function<T>(separator:cs.Char16, values:cs.system.collections.generic.IEnumerable<T>):cs.system.text.StringBuilder {})
	/**
	 * @param separator 
	 * @param values 
	 */
	function AppendJoin<T>(separator:String, values:cs.system.collections.generic.IEnumerable<T>):cs.system.text.StringBuilder;
	@:overload(function():cs.system.text.StringBuilder {})
	/**
	 * Appends the default line terminator to the end of the current  object.
	 * @return A reference to this instance after the append operation has completed.
	 */
	function AppendLine(value:String):cs.system.text.StringBuilder;
	/**
	 * Removes all characters from the current  instance.
	 * @return An object whose  is 0 (zero).
	 */
	function Clear():cs.system.text.StringBuilder;
	@:overload(function(sourceIndex:Int, destination:cs.system.Span<cs.Char16>, count:Int):Void {})
	/**
	 * Copies the characters from a specified segment of this instance to a specified
	 * segment of a destination  array.
	 * @param sourceIndex The starting position in this instance where characters will
	 * be copied from. The index is zero-based.
	 * @param destination The array where characters will be copied.
	 * @param destinationIndex The starting position in  where characters will be
	 * copied. The index is zero-based.
	 * @param count The number of characters to be copied.
	 */
	function CopyTo(sourceIndex:Int, destination:cs.NativeArray<cs.Char16>, destinationIndex:Int, count:Int):Void;
	/**
	 * Ensures that the capacity of this instance of  is at least the specified value.
	 * @param capacity The minimum capacity to ensure.
	 * @return The new capacity of this instance.
	 */
	function EnsureCapacity(capacity:Int):Int;
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>):Bool {})
	/**
	 * Returns a value indicating whether the characters in this instance are equal to
	 * the characters in a specified read-only character span.
	 * @param span The character span to compare with the current instance.
	 * @return if the characters in this instance and  are the same; otherwise, .
	 */
	function Equals(sb:cs.system.text.StringBuilder):Bool;
	@:overload(function(index:Int, value:Bool):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.UInt8):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.Char16):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.NativeArray<cs.Char16>):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.system.Decimal):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:Float):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.Int16):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:Int):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:haxe.Int64):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:Dynamic):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.system.ReadOnlySpan<cs.Char16>):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.Int8):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:Single):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:String):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.UInt16):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.UInt):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:cs.UInt64):cs.system.text.StringBuilder {})
	@:overload(function(index:Int, value:String, count:Int):cs.system.text.StringBuilder {})
	/**
	 * Inserts the string representation of a Boolean value into this instance at the
	 * specified character position.
	 * @param index The position in this instance where insertion begins.
	 * @param value The value to insert.
	 * @return A reference to this instance after the insert operation has completed.
	 */
	function Insert(index:Int, value:cs.NativeArray<cs.Char16>, startIndex:Int, charCount:Int):cs.system.text.StringBuilder;
	/**
	 * Removes the specified range of characters from this instance.
	 * @param startIndex The zero-based position in this instance where removal begins.
	 * @param length The number of characters to remove.
	 * @return A reference to this instance after the excise operation has completed.
	 */
	function Remove(startIndex:Int, length:Int):cs.system.text.StringBuilder;
	@:overload(function(oldChar:cs.Char16, newChar:cs.Char16):cs.system.text.StringBuilder {})
	@:overload(function(oldValue:String, newValue:String):cs.system.text.StringBuilder {})
	@:overload(function(oldChar:cs.Char16, newChar:cs.Char16, startIndex:Int, count:Int):cs.system.text.StringBuilder {})
	/**
	 * Replaces all occurrences of a specified character in this instance with another
	 * specified character.
	 * @param oldChar The character to replace.
	 * @param newChar The character that replaces .
	 * @return A reference to this instance with  replaced by .
	 */
	function Replace(oldValue:String, newValue:String, startIndex:Int, count:Int):cs.system.text.StringBuilder;
	@:overload(function():String {})
	/**
	 * Converts the value of this instance to a .
	 * @return A string whose value is the same as this instance.
	 */
	function ToString(startIndex:Int, length:Int):String;
}
