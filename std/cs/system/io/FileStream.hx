package cs.system.io;

@:native('System.IO.FileStream') extern class FileStream extends Stream
{
	@:overload(function (path:String, mode:FileMode, access:FileAccess, share:FileShare, bufferSize:Int, useAsync:Bool):Void {})
	@:overload(function (path:String, mode:FileMode, access:FileAccess, share:FileShare, bufferSize:Int):Void {})
	@:overload(function (path:String, mode:FileMode, access:FileAccess, share:FileShare):Void {})
	@:overload(function (path:String, mode:FileMode, access:FileAccess):Void {})
	function new(path:String, mode:FileMode):Void;
	var IsAsync(default, null):Bool;
	var Name(default, null):String;
}