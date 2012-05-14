package java.lang;

extern class StringBuilder 
{

	function new():Void;
	
	@:overload(function(obj:Dynamic):Void {})
	@:overload(function(str:String, start:Int, end:Int):Void {})
	function append(char:java.StdTypes.Char16):Void;
	
	function toString():String;
}