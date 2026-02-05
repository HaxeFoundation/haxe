package cs.system.net;

/** The exception that is thrown when an error occurs processing an HTTP request. */
@:native("System.Net.HttpListenerException")
extern class HttpListenerException extends cs.system.componentmodel.Win32Exception {
	@:overload(function():Void {})
	@:overload(function(errorCode:Int):Void {})
	function new(errorCode:Int, message:String):Void;
}
