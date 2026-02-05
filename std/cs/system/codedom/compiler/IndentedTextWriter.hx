package cs.system.codedom.compiler;

/** Provides a text writer that can indent new lines by a tab string token. */
@:native("System.CodeDom.Compiler.IndentedTextWriter")
extern class IndentedTextWriter extends cs.system.io.TextWriter {
	/** Specifies the default tab string. This field is constant. */
	static var DefaultTabString(default, never):String;
	/**
	 * Gets or sets the number of spaces to indent.
	 * @return The number of spaces to indent.
	 */
	var Indent(default, default):Int;
	/**
	 * Gets the  to use.
	 * @return The  to use.
	 */
	var InnerWriter(default, never):cs.system.io.TextWriter;
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	function new(writer:cs.system.io.TextWriter, tabString:String):Void;
	/** Closes the document being written to. */
	function Close():Void;
	/** Flushes the stream. */
	function Flush():Void;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(s:String):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	/**
	 * Writes the text representation of a Boolean value to the text stream.
	 * @param value The Boolean value to write.
	 */
	function Write(format:String, arg0:Dynamic, arg1:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(s:String):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	/** Writes a line terminator. */
	function WriteLine(format:String, arg0:Dynamic, arg1:Dynamic):Void;
	/**
	 * Writes the specified string to a line without tabs.
	 * @param s The string to write.
	 */
	function WriteLineNoTabs(s:String):Void;
}
