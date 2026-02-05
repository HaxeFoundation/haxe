package cs.system;

/** The exception that is thrown when there is an internal error in the execution engine of the common language runtime. This class cannot be inherited. */
@:native("System.ExecutionEngineException")
extern class ExecutionEngineException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
