package cs.system.io;

/** Reads primitive data types as binary values in a specific encoding. */
@:native("System.IO.BinaryReader")
extern class BinaryReader {
	/**
	 * Exposes access to the underlying stream of the .
	 * @return The underlying stream associated with the .
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	@:overload(function(input:cs.system.io.Stream):Void {})
	@:overload(function(input:cs.system.io.Stream, encoding:cs.system.text.Encoding):Void {})
	function new(input:cs.system.io.Stream, encoding:cs.system.text.Encoding, leaveOpen:Bool):Void;
	/** Closes the current reader and the underlying stream. */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Returns the next available character and does not advance the byte or character
	 * position.
	 * @return The next available character, or -1 if no more characters are available
	 * or the stream does not support seeking.
	 */
	function PeekChar():Int;
	@:overload(function():Int {})
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int {})
	/**
	 * Reads characters from the underlying stream and advances the current position of
	 * the stream in accordance with the  used and the specific character being read
	 * from the stream.
	 * @return The next character from the input stream, or -1 if no characters are
	 * currently available.
	 */
	function Read(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	/**
	 * Reads a  value from the current stream and advances the current position of the
	 * stream by one byte.
	 * @return if the byte is nonzero; otherwise, .
	 */
	function ReadBoolean():Bool;
	/**
	 * Reads the next byte from the current stream and advances the current position of
	 * the stream by one byte.
	 * @return The next byte read from the current stream.
	 */
	function ReadByte():cs.UInt8;
	/**
	 * Reads the specified number of bytes from the current stream into a byte array
	 * and advances the current position by that number of bytes.
	 * @param count The number of bytes to read. This value must be 0 or a non-negative
	 * number or an exception will occur.
	 * @return A byte array containing data read from the underlying stream. This might
	 * be less than the number of bytes requested if the end of the stream is reached.
	 */
	function ReadBytes(count:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Reads the next character from the current stream and advances the current
	 * position of the stream in accordance with the  used and the specific character
	 * being read from the stream.
	 * @return A character read from the current stream.
	 */
	function ReadChar():cs.Char16;
	/**
	 * Reads the specified number of characters from the current stream, returns the
	 * data in a character array, and advances the current position in accordance with
	 * the  used and the specific character being read from the stream.
	 * @param count The number of characters to read.
	 * @return A character array containing data read from the underlying stream. This
	 * might be less than the number of characters requested if the end of the stream
	 * is reached.
	 */
	function ReadChars(count:Int):cs.NativeArray<cs.Char16>;
	/**
	 * Reads a decimal value from the current stream and advances the current position
	 * of the stream by sixteen bytes.
	 * @return A decimal value read from the current stream.
	 */
	function ReadDecimal():cs.system.Decimal;
	/**
	 * Reads an 8-byte floating point value from the current stream and advances the
	 * current position of the stream by eight bytes.
	 * @return An 8-byte floating point value read from the current stream.
	 */
	function ReadDouble():Float;
	/**
	 * Reads a 2-byte signed integer from the current stream and advances the current
	 * position of the stream by two bytes.
	 * @return A 2-byte signed integer read from the current stream.
	 */
	function ReadInt16():cs.Int16;
	/**
	 * Reads a 4-byte signed integer from the current stream and advances the current
	 * position of the stream by four bytes.
	 * @return A 4-byte signed integer read from the current stream.
	 */
	function ReadInt32():Int;
	/**
	 * Reads an 8-byte signed integer from the current stream and advances the current
	 * position of the stream by eight bytes.
	 * @return An 8-byte signed integer read from the current stream.
	 */
	function ReadInt64():haxe.Int64;
	/**
	 * Reads a signed byte from this stream and advances the current position of the
	 * stream by one byte.
	 * @return A signed byte read from the current stream.
	 */
	function ReadSByte():cs.Int8;
	/**
	 * Reads a 4-byte floating point value from the current stream and advances the
	 * current position of the stream by four bytes.
	 * @return A 4-byte floating point value read from the current stream.
	 */
	function ReadSingle():Single;
	/**
	 * Reads a string from the current stream. The string is prefixed with the length,
	 * encoded as an integer seven bits at a time.
	 * @return The string being read.
	 */
	function ReadString():String;
	/**
	 * Reads a 2-byte unsigned integer from the current stream using little-endian
	 * encoding and advances the position of the stream by two bytes.
	 * @return A 2-byte unsigned integer read from this stream.
	 */
	function ReadUInt16():cs.UInt16;
	/**
	 * Reads a 4-byte unsigned integer from the current stream and advances the
	 * position of the stream by four bytes.
	 * @return A 4-byte unsigned integer read from this stream.
	 */
	function ReadUInt32():cs.UInt;
	/**
	 * Reads an 8-byte unsigned integer from the current stream and advances the
	 * position of the stream by eight bytes.
	 * @return An 8-byte unsigned integer read from this stream.
	 */
	function ReadUInt64():cs.UInt64;
}
