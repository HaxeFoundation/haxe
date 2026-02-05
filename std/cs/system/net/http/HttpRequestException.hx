package cs.system.net.http;

/** A base class for exceptions thrown by the  and  classes. */
@:native("System.Net.Http.HttpRequestException")
extern class HttpRequestException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
