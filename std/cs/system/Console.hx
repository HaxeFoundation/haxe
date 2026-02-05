package cs.system;

/** Represents the standard input, output, and error streams for console applications. This class cannot be inherited. */
@:native("System.Console")
extern class Console {
	/**
	 * Gets or sets the background color of the console.
	 * @return A value that specifies the background color of the console; that is, the
	 * color that appears behind each character. The default is black.
	 */
	static var BackgroundColor(default, default):cs.system.ConsoleColor;
	/**
	 * Gets or sets the height of the buffer area.
	 * @return The current height, in rows, of the buffer area.
	 */
	static var BufferHeight(default, default):Int;
	/**
	 * Gets or sets the width of the buffer area.
	 * @return The current width, in columns, of the buffer area.
	 */
	static var BufferWidth(default, default):Int;
	/**
	 * Gets a value indicating whether the CAPS LOCK keyboard toggle is turned on or
	 * turned off.
	 * @return if CAPS LOCK is turned on;  if CAPS LOCK is turned off.
	 */
	static var CapsLock(default, never):Bool;
	/**
	 * Gets or sets the column position of the cursor within the buffer area.
	 * @return The current position, in columns, of the cursor.
	 */
	static var CursorLeft(default, default):Int;
	/**
	 * Gets or sets the height of the cursor within a character cell.
	 * @return The size of the cursor expressed as a percentage of the height of a
	 * character cell. The property value ranges from 1 to 100.
	 */
	static var CursorSize(default, default):Int;
	/**
	 * Gets or sets the row position of the cursor within the buffer area.
	 * @return The current position, in rows, of the cursor.
	 */
	static var CursorTop(default, default):Int;
	/**
	 * Gets or sets a value indicating whether the cursor is visible.
	 * @return if the cursor is visible; otherwise, .
	 */
	static var CursorVisible(default, default):Bool;
	/**
	 * Gets the standard error output stream.
	 * @return A  that represents the standard error output stream.
	 */
	static var Error(default, never):cs.system.io.TextWriter;
	/**
	 * Gets or sets the foreground color of the console.
	 * @return A  that specifies the foreground color of the console; that is, the
	 * color of each character that is displayed. The default is gray.
	 */
	static var ForegroundColor(default, default):cs.system.ConsoleColor;
	/**
	 * Gets the standard input stream.
	 * @return A  that represents the standard input stream.
	 */
	static var In(default, never):cs.system.io.TextReader;
	/**
	 * Gets or sets the encoding the console uses to read input.
	 * @return The encoding used to read console input.
	 */
	static var InputEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets a value that indicates whether the error output stream has been redirected
	 * from the standard error stream.
	 * @return if error output is redirected; otherwise, .
	 */
	static var IsErrorRedirected(default, never):Bool;
	/**
	 * Gets a value that indicates whether input has been redirected from the standard
	 * input stream.
	 * @return if input is redirected; otherwise, .
	 */
	static var IsInputRedirected(default, never):Bool;
	/**
	 * Gets a value that indicates whether output has been redirected from the standard
	 * output stream.
	 * @return if output is redirected; otherwise, .
	 */
	static var IsOutputRedirected(default, never):Bool;
	/**
	 * Gets a value indicating whether a key press is available in the input stream.
	 * @return if a key press is available; otherwise, .
	 */
	static var KeyAvailable(default, never):Bool;
	/**
	 * Gets the largest possible number of console window rows, based on the current
	 * font and screen resolution.
	 * @return The height of the largest possible console window measured in rows.
	 */
	static var LargestWindowHeight(default, never):Int;
	/**
	 * Gets the largest possible number of console window columns, based on the current
	 * font and screen resolution.
	 * @return The width of the largest possible console window measured in columns.
	 */
	static var LargestWindowWidth(default, never):Int;
	/**
	 * Gets a value indicating whether the NUM LOCK keyboard toggle is turned on or
	 * turned off.
	 * @return if NUM LOCK is turned on;  if NUM LOCK is turned off.
	 */
	static var NumberLock(default, never):Bool;
	/**
	 * Gets the standard output stream.
	 * @return A  that represents the standard output stream.
	 */
	static var Out(default, never):cs.system.io.TextWriter;
	/**
	 * Gets or sets the encoding the console uses to write output.
	 * @return The encoding used to write console output.
	 */
	static var OutputEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the title to display in the console title bar.
	 * @return The string to be displayed in the title bar of the console. The maximum
	 * length of the title string is 24500 characters.
	 */
	static var Title(default, default):String;
	/**
	 * Gets or sets a value indicating whether the combination of the  modifier key and
	 * console key (Ctrl+C) is treated as ordinary input or as an interruption that is
	 * handled by the operating system.
	 * @return if Ctrl+C is treated as ordinary input; otherwise, .
	 */
	static var TreatControlCAsInput(default, default):Bool;
	/**
	 * Gets or sets the height of the console window area.
	 * @return The height of the console window measured in rows.
	 */
	static var WindowHeight(default, default):Int;
	/**
	 * Gets or sets the leftmost position of the console window area relative to the
	 * screen buffer.
	 * @return The leftmost console window position measured in columns.
	 */
	static var WindowLeft(default, default):Int;
	/**
	 * Gets or sets the top position of the console window area relative to the screen
	 * buffer.
	 * @return The uppermost console window position measured in rows.
	 */
	static var WindowTop(default, default):Int;
	/**
	 * Gets or sets the width of the console window.
	 * @return The width of the console window measured in columns.
	 */
	static var WindowWidth(default, default):Int;
	@:overload(function():Void {})
	/** Plays the sound of a beep through the console speaker. */
	static function Beep(frequency:Int, duration:Int):Void;
	/** Clears the console buffer and corresponding console window of display information. */
	static function Clear():Void;
	@:overload(function(sourceLeft:Int, sourceTop:Int, sourceWidth:Int, sourceHeight:Int, targetLeft:Int, targetTop:Int):Void {})
	/**
	 * Copies a specified source area of the screen buffer to a specified destination
	 * area.
	 * @param sourceLeft The leftmost column of the source area.
	 * @param sourceTop The topmost row of the source area.
	 * @param sourceWidth The number of columns in the source area.
	 * @param sourceHeight The number of rows in the source area.
	 * @param targetLeft The leftmost column of the destination area.
	 * @param targetTop The topmost row of the destination area.
	 */
	static function MoveBufferArea(sourceLeft:Int, sourceTop:Int, sourceWidth:Int, sourceHeight:Int, targetLeft:Int, targetTop:Int, sourceChar:cs.Char16, sourceForeColor:cs.system.ConsoleColor, sourceBackColor:cs.system.ConsoleColor):Void;
	@:overload(function():cs.system.io.Stream {})
	/**
	 * Acquires the standard error stream.
	 * @return The standard error stream.
	 */
	static function OpenStandardError(bufferSize:Int):cs.system.io.Stream;
	@:overload(function():cs.system.io.Stream {})
	/**
	 * Acquires the standard input stream.
	 * @return The standard input stream.
	 */
	static function OpenStandardInput(bufferSize:Int):cs.system.io.Stream;
	@:overload(function():cs.system.io.Stream {})
	/**
	 * Acquires the standard output stream.
	 * @return The standard output stream.
	 */
	static function OpenStandardOutput(bufferSize:Int):cs.system.io.Stream;
	/**
	 * Reads the next character from the standard input stream.
	 * @return The next character from the input stream, or negative one (-1) if there
	 * are currently no more characters to be read.
	 */
	static function Read():Int;
	@:overload(function():cs.system.ConsoleKeyInfo {})
	/**
	 * Obtains the next character or function key pressed by the user. The pressed key
	 * is displayed in the console window.
	 * @return An object that describes the  constant and Unicode character, if any,
	 * that correspond to the pressed console key. The  object also describes, in a
	 * bitwise combination of  values, whether one or more Shift, Alt, or Ctrl modifier
	 * keys was pressed simultaneously with the console key.
	 */
	static function ReadKey(intercept:Bool):cs.system.ConsoleKeyInfo;
	/**
	 * Reads the next line of characters from the standard input stream.
	 * @return The next line of characters from the input stream, or  if no more lines
	 * are available.
	 */
	static function ReadLine():String;
	/** Sets the foreground and background console colors to their defaults. */
	static function ResetColor():Void;
	/**
	 * Sets the height and width of the screen buffer area to the specified values.
	 * @param width The width of the buffer area measured in columns.
	 * @param height The height of the buffer area measured in rows.
	 */
	static function SetBufferSize(width:Int, height:Int):Void;
	/**
	 * Sets the position of the cursor.
	 * @param left The column position of the cursor. Columns are numbered from left to
	 * right starting at 0.
	 * @param top The row position of the cursor. Rows are numbered from top to bottom
	 * starting at 0.
	 */
	static function SetCursorPosition(left:Int, top:Int):Void;
	/**
	 * Sets the  property to the specified  object.
	 * @param newError A stream that is the new standard error output.
	 */
	static function SetError(newError:cs.system.io.TextWriter):Void;
	/**
	 * Sets the  property to the specified  object.
	 * @param newIn A stream that is the new standard input.
	 */
	static function SetIn(newIn:cs.system.io.TextReader):Void;
	/**
	 * Sets the  property to target the  object.
	 * @param newOut A text writer to be used as the new standard output.
	 */
	static function SetOut(newOut:cs.system.io.TextWriter):Void;
	/**
	 * Sets the position of the console window relative to the screen buffer.
	 * @param left The column position of the upper left  corner of the console window.
	 * @param top The row position of the upper left corner of the console window.
	 */
	static function SetWindowPosition(left:Int, top:Int):Void;
	/**
	 * Sets the height and width of the console window to the specified values.
	 * @param width The width of the console window measured in columns.
	 * @param height The height of the console window measured in rows.
	 */
	static function SetWindowSize(width:Int, height:Int):Void;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic):Void {})
	/**
	 * Writes the text representation of the specified Boolean value to the standard
	 * output stream.
	 * @param value The value to write.
	 */
	static function Write(format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic):Void {})
	/** Writes the current line terminator to the standard output stream. */
	static function WriteLine(format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):Void;
}
