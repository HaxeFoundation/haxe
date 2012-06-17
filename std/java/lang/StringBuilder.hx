package java.lang;

extern class StringBuilder 
{

	function new():Void;
	
	@:overload(function(char:java.StdTypes.Char16):Void {})
	@:overload(function(str:String, start:Int, end:Int):Void {})
	function append(obj:Dynamic):Void;
	
	function toString():String;
}