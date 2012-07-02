package java.io;

extern class PrintStream extends OutputStream
{
	@:overload(function(out:OutputStream, autoFlush:Bool):Void {})
	function new(file:File):Void;
	
	function append(s:String):PrintStream;
	function format(s:String, args:java.NativeArray<Dynamic>):PrintStream;
	function printf(s:String, args:java.NativeArray<Dynamic>):PrintStream;
	function print(obj:Dynamic):Void;
	function println(obj:Dynamic):Void;
}