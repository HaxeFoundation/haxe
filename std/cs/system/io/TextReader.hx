package cs.system.io;

/** Represents a reader that can read a sequential series of characters. */
@:native("System.IO.TextReader")
extern class TextReader extends cs.system.MarshalByRefObject {
	/** Provides a  with no data to read from. */
	static var Null(default, never):cs.system.io.TextReader;
	/**
	 * Creates a thread-safe wrapper around the specified .
	 * @param reader The  to synchronize.
	 * @return A thread-safe .
	 */
	static function Synchronized(reader:cs.system.io.TextReader):cs.system.io.TextReader;
	/** Closes the  and releases any system resources associated with the . */
	function Close():Void;
	/** Releases all resources used by the  object. */
	function Dispose():Void;
	/**
	 * Reads the next character without changing the state of the reader or the
	 * character source. Returns the next available character without actually reading
	 * it from the reader.
	 * @return An integer representing the next character to be read, or -1 if no more
	 * characters are available or the reader does not support seeking.
	 */
	function Peek():Int;
	@:overload(function():Int {})
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	/**
	 * Reads the next character from the text reader and advances the character
	 * position by one character.
	 * @return The next character from the text reader, or -1 if no more characters are
	 * available. The default implementation returns -1.
	 */
	function Read(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current text reader
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the text is
	 * reached before the specified number of characters is read into the buffer, the
	 * current method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the text has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	@:overload(function(buffer:cs.system.Span<cs.Char16>):Int {})
	/**
	 * Reads a specified maximum number of characters from the current text reader and
	 * writes the data to a buffer, beginning at the specified index.
	 * @param buffer When this method returns, this parameter contains the specified
	 * character array with the values between  and ( +  -1) replaced by the characters
	 * read from the current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read.
	 * @return The number of characters that have been read. The number will be less
	 * than or equal to , depending on whether all input characters have been read.
	 */
	function ReadBlock(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads a specified maximum number of characters from the current text reader
	 * asynchronously and writes the data to a buffer, beginning at the specified
	 * index.
	 * @param buffer When this method returns, contains the specified character array
	 * with the values between  and ( +  - 1) replaced by the characters read from the
	 * current source.
	 * @param index The position in  at which to begin writing.
	 * @param count The maximum number of characters to read. If the end of the text is
	 * reached before the specified number of characters is read into the buffer, the
	 * current method returns.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the text has been reached.
	 */
	function ReadBlockAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a line of characters from the text reader and returns the data as a
	 * string.
	 * @return The next line from the reader, or  if all characters have been read.
	 */
	function ReadLine():String;
	/**
	 * Reads a line of characters asynchronously and returns the data as a string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the next line from the text reader, or is  if all of the
	 * characters have been read.
	 */
	function ReadLineAsync():cs.system.threading.tasks.Task_1<String>;
	/**
	 * Reads all characters from the current position to the end of the text reader and
	 * returns them as one string.
	 * @return A string that contains all characters from the current position to the
	 * end of the text reader.
	 */
	function ReadToEnd():String;
	/**
	 * Reads all characters from the current position to the end of the text reader
	 * asynchronously and returns them as one string.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains a string with the characters from the current position to the
	 * end of the text reader.
	 */
	function ReadToEndAsync():cs.system.threading.tasks.Task_1<String>;
}
