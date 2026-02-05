package cs.system.io.isolatedstorage;

/** The exception that is thrown when an operation in isolated storage fails. */
@:native("System.IO.IsolatedStorage.IsolatedStorageException")
extern class IsolatedStorageException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
