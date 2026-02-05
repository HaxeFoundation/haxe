package cs.system.io;

/** Implements a  that reads characters from a byte stream in a particular encoding. */
@:native("System.IO.StreamReader")
extern class StreamReader extends cs.system.io.TextReader {
	/** A  object around an empty stream. */
	static var Null(default, never):cs.system.io.StreamReader;
	/**
	 * Returns the underlying stream.
	 * @return The underlying stream.
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	/**
	 * Gets the current character encoding that the current  object is using.
	 * @return The current character encoding used by the current reader. The value can
	 * be different after the first call to any  method of , since encoding
	 * autodetection is not done until the first call to a  method.
	 */
	var CurrentEncoding(default, never):cs.system.text.Encoding;
	/**
	 * Gets a value that indicates whether the current stream position is at the end of
	 * the stream.
	 * @return if the current stream position is at the end of the stream; otherwise .
	 */
	var EndOfStream(default, never):Bool;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(path:String):Void {})
	@:overload(function(stream:cs.system.io.Stream, detectEncodingFromByteOrderMarks:Bool):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding):Void {})
	@:overload(function(path:String, detectEncodingFromByteOrderMarks:Bool):Void {})
	@:overload(function(path:String, encoding:cs.system.text.Encoding):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, detectEncodingFromByteOrderMarks:Bool):Void {})
	@:overload(function(path:String, encoding:cs.system.text.Encoding, detectEncodingFromByteOrderMarks:Bool):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, detectEncodingFromByteOrderMarks:Bool, bufferSize:Int):Void {})
	@:overload(function(path:String, encoding:cs.system.text.Encoding, detectEncodingFromByteOrderMarks:Bool, bufferSize:Int):Void {})
	function new(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, detectEncodingFromByteOrderMarks:Bool, bufferSize:Int, leaveOpen:Bool):Void;
	/** Closes the  object and the underlying stream, and releases any system resources associated with the reader. */
	function Close():Void;
	/** Clears the internal buffer. */
	function DiscardBufferedData():Void;
	/**
	 * Returns the next available character but does not consume it.
	 * @return An integer representing the next character to be read, or -1 if there
	 * are no characters to be read or if the stream does not support seeking.
	 */
	function Peek():Int;
	@:overload(function():Int {})
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	/**
	 * Reads the next character from the input stream and advances the character
	 * position by one character.
	 * @return The next character from the input stream represented as an  object, or
	 * -1 if no more characters are available.
	 */
	function Read(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current stream
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the stream
	 * is reached before the specified number of characters is written into the buffer,
	 * the current method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of characters read into the buffer. The
	 * result value can be less than the number of characters requested if the number
	 * of characters currently available is less than the requested number, or it can
	 * be 0 (zero) if the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	/**
	 * Reads a specified maximum number of characters from the current stream and
	 * writes the data to a buffer, beginning at the specified index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and (index + count - 1) replaced by the characters read
	 * from the current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read.
	 * @return The number of characters that have been read. The number will be less
	 * than or equal to , depending on whether all input characters have been read.
	 */
	function ReadBlock(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current stream
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the stream
	 * is reached before the specified number of characters is written into the buffer,
	 * the method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of characters read into the buffer. The
	 * result value can be less than the number of characters requested if the number
	 * of characters currently available is less than the requested number, or it can
	 * be 0 (zero) if the end of the stream has been reached.
	 */
	function ReadBlockAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a line of characters from the current stream and returns the data as a
	 * string.
	 * @return The next line from the input stream, or  if the end of the input stream
	 * is reached.
	 */
	function ReadLine():String;
	/**
	 * Reads a line of characters asynchronously from the current stream and returns
	 * the data as a string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the next line from the stream, or is  if all the characters
	 * have been read.
	 */
	function ReadLineAsync():cs.system.threading.tasks.Task_1<String>;
	/**
	 * Reads all characters from the current position to the end of the stream.
	 * @return The rest of the stream as a string, from the current position to the
	 * end. If the current position is at the end of the stream, returns an empty
	 * string ("").
	 */
	function ReadToEnd():String;
	/**
	 * Reads all characters from the current position to the end of the stream
	 * asynchronously and returns them as one string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains a string with the characters from the current position to the
	 * end of the stream.
	 */
	function ReadToEndAsync():cs.system.threading.tasks.Task_1<String>;
}
