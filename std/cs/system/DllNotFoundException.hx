package cs.system;

/** The exception that is thrown when a DLL specified in a DLL import cannot be found. */
@:native("System.DllNotFoundException")
extern class DllNotFoundException extends cs.system.TypeLoadException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
