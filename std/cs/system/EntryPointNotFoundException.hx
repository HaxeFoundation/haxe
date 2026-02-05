package cs.system;

/** The exception that is thrown when an attempt to load a class fails due to the absence of an entry method. */
@:native("System.EntryPointNotFoundException")
extern class EntryPointNotFoundException extends cs.system.TypeLoadException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
