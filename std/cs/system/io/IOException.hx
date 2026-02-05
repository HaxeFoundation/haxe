package cs.system.io;

/** The exception that is thrown when an I/O error occurs. */
@:native("System.IO.IOException")
extern class IOException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, hresult:Int):Void;
}
