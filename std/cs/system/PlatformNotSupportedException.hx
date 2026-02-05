package cs.system;

/** The exception that is thrown when a feature does not run on a particular platform. */
@:native("System.PlatformNotSupportedException")
extern class PlatformNotSupportedException extends cs.system.NotSupportedException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
