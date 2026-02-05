package cs.system.runtime.exceptionservices;

/** Enables managed code to handle exceptions that indicate a corrupted process state. */
@:native("System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptionsAttribute")
extern class HandleProcessCorruptedStateExceptionsAttribute extends cs.system.Attribute {
	function new():Void;
}
