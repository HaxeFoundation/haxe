package cs.system.threading.tasks;

@:native("System.Threading.Tasks.TaskCompletionSource")
extern class TaskCompletionSource<TResult> {
	var Task(default, never):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function():Void {})
	@:overload(function(state:Dynamic):Void {})
	@:overload(function(creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	function new(state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void;
	function SetCanceled():Void;
	@:overload(function(exceptions:cs.system.collections.generic.IEnumerable<cs.system.Exception>):Void {})
	function SetException(exception:cs.system.Exception):Void;
	function SetResult(result:TResult):Void;
	@:overload(function():Bool {})
	function TrySetCanceled(cancellationToken:cs.system.threading.CancellationToken):Bool;
	@:overload(function(exceptions:cs.system.collections.generic.IEnumerable<cs.system.Exception>):Bool {})
	function TrySetException(exception:cs.system.Exception):Bool;
	function TrySetResult(result:TResult):Bool;
}
