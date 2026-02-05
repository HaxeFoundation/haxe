package cs.system.io;

/** Implements a  that reads from a string. */
@:native("System.IO.StringReader")
extern class StringReader extends cs.system.io.TextReader {
	function new(s:String):Void;
	/** Closes the . */
	function Close():Void;
	/**
	 * Returns the next available character but does not consume it.
	 * @return An integer representing the next character to be read, or -1 if no more
	 * characters are available or the stream does not support seeking.
	 */
	function Peek():Int;
	@:overload(function():Int {})
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	/**
	 * Reads the next character from the input string and advances the character
	 * position by one character.
	 * @return The next character from the underlying string, or -1 if no more
	 * characters are available.
	 */
	function Read(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current string
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the string
	 * is reached before the specified number of characters is written into the buffer,
	 * the method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the string has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/** @param buffer  */
	function ReadBlock(buffer:cs.system.Span<cs.Char16>):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current string
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the string
	 * is reached before the specified number of characters is written into the buffer,
	 * the method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the string has been reached.
	 */
	function ReadBlockAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a line of characters from the current string and returns the data as a
	 * string.
	 * @return The next line from the current string, or  if the end of the string is
	 * reached.
	 */
	function ReadLine():String;
	/**
	 * Reads a line of characters asynchronously from the current string and returns
	 * the data as a string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the next line from the string reader, or is  if all the
	 * characters have been read.
	 */
	function ReadLineAsync():cs.system.threading.tasks.Task_1<String>;
	/**
	 * Reads all characters from the current position to the end of the string and
	 * returns them as a single string.
	 * @return The content from the current position to the end of the underlying
	 * string.
	 */
	function ReadToEnd():String;
	/**
	 * Reads all characters from the current position to the end of the string
	 * asynchronously and returns them as a single string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains a string with the characters from the current position to the
	 * end of the string.
	 */
	function ReadToEndAsync():cs.system.threading.tasks.Task_1<String>;
}
