package cs.system.io;

/** The exception that is thrown when part of a file or directory cannot be found. */
@:native("System.IO.DirectoryNotFoundException")
extern class DirectoryNotFoundException extends cs.system.io.IOException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
