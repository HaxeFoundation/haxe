package cs.system.componentmodel;

/** Thrown when a thread on which an operation should execute no longer exists or has no message loop. */
@:native("System.ComponentModel.InvalidAsynchronousStateException")
extern class InvalidAsynchronousStateException extends cs.system.ArgumentException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
