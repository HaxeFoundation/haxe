package cs.system.threading.tasks;

/** Represents an exception used to communicate an invalid operation by a . */
@:native("System.Threading.Tasks.TaskSchedulerException")
extern class TaskSchedulerException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(innerException:cs.system.Exception):Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
