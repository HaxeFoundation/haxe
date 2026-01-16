package cs.system;

@:native("System.Exception")
extern class Exception {
	var Message(default, never):String;
	var StackTrace(default, never):String;
	var InnerException(default, never):Exception;

	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:Exception):Void;

	function ToString():String;
	function GetBaseException():Exception;
}
