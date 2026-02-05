package cs.system.io;

/** The exception that is thrown when trying to access a drive or share that is not available. */
@:native("System.IO.DriveNotFoundException")
extern class DriveNotFoundException extends cs.system.io.IOException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
