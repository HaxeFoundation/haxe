package cs.system.runtime;

@:native("System.Runtime.AmbiguousImplementationException")
extern class AmbiguousImplementationException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
