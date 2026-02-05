package cs.system.threading.tasks;

/** Represents an asynchronous operation. */
@:native("System.Threading.Tasks.Task`1")
extern class Task_1<TResult> extends cs.system.threading.tasks.Task {
	var Result(default, never):TResult;
	@:overload(function(function_:cs.system.Func_1<TResult>):Void {})
	@:overload(function(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic):Void {})
	@:overload(function(function_:cs.system.Func_1<TResult>, cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(function_:cs.system.Func_1<TResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	@:overload(function(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	@:overload(function(function_:cs.system.Func_1<TResult>, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	function new(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void;
	/**
	 * Configures an awaiter used to await this .
	 * @param continueOnCapturedContext to attempt to marshal the continuation back to
	 * the original context captured; otherwise, .
	 * @return An object used to await this task.
	 */
	function ConfigureAwait(continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredTaskAwaitable_1<TResult>;
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TResult>, TNewResult>):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task_1<TResult>, Dynamic>, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TResult>>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TResult>>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TResult>>, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task_1<TResult>, Dynamic, TNewResult>, state:Dynamic):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TResult>, TNewResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TResult>, TNewResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TResult>, TNewResult>, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task_1<TResult>, Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task_1<TResult>, Dynamic>, state:Dynamic, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task_1<TResult>, Dynamic>, state:Dynamic, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task_1<TResult>, Dynamic, TNewResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task_1<TResult>, Dynamic, TNewResult>, state:Dynamic, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task_1<TResult>, Dynamic, TNewResult>, state:Dynamic, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TResult>>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TNewResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TResult>, TNewResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TNewResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task_1<TResult>, Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	/**
	 * Creates a continuation that receives caller-supplied state information and
	 * executes when the target  completes.
	 * @param continuationAction An action to run when the task completes. When run,
	 * the delegate is passed the completed task and a caller-supplied state object as
	 * arguments.
	 * @param state An object representing data to be used by the continuation action.
	 * @return A new continuation task.
	 */
	function ContinueWith<TNewResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task_1<TResult>, Dynamic, TNewResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TNewResult>;
	/**
	 * Gets an awaiter used to await this .
	 * @return An awaiter instance.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.TaskAwaiter_1<TResult>;
}
