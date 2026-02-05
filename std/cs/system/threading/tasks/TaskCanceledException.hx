package cs.system.threading.tasks;

/** Represents an exception used to communicate task cancellation. */
@:native("System.Threading.Tasks.TaskCanceledException")
extern class TaskCanceledException extends cs.system.OperationCanceledException {
	/**
	 * Gets the task associated with this exception.
	 * @return A reference to the  that is associated with this exception.
	 */
	var Task(default, never):cs.system.threading.tasks.Task;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(task:cs.system.threading.tasks.Task):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerException:cs.system.Exception, token:cs.system.threading.CancellationToken):Void;
}
