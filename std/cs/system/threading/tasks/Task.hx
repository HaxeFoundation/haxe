package cs.system.threading.tasks;

/** Represents an asynchronous operation. */
@:native("System.Threading.Tasks.Task")
extern class Task {
	/**
	 * Gets a task that has already completed successfully.
	 * @return The successfully completed task.
	 */
	static var CompletedTask(default, never):cs.system.threading.tasks.Task;
	/**
	 * Returns the ID of the currently executing .
	 * @return An integer that was assigned by the system to the currently-executing
	 * task.
	 */
	static var CurrentId(default, never):Null<Int>;
	/**
	 * Provides access to factory methods for creating and configuring  and  instances.
	 * @return A factory object that can create a variety of  and  objects.
	 */
	static var Factory(default, never):cs.system.threading.tasks.TaskFactory;
	/**
	 * Gets the state object supplied when the  was created, or null if none was
	 * supplied.
	 * @return An  that represents the state data that was passed in to the task when
	 * it was created.
	 */
	var AsyncState(default, never):Dynamic;
	/**
	 * Gets the  used to create this task.
	 * @return The  used to create this task.
	 */
	var CreationOptions(default, never):cs.system.threading.tasks.TaskCreationOptions;
	/**
	 * Gets the  that caused the  to end prematurely. If the  completed successfully or
	 * has not yet thrown any exceptions, this will return .
	 * @return The  that caused the  to end prematurely.
	 */
	var Exception(default, never):cs.system.AggregateException;
	/**
	 * Gets an ID for this  instance.
	 * @return The identifier that is assigned by the system to this  instance.
	 */
	var Id(default, never):Int;
	/**
	 * Gets whether this  instance has completed execution due to being canceled.
	 * @return if the task has completed due to being canceled; otherwise .
	 */
	var IsCanceled(default, never):Bool;
	/**
	 * Gets a value that indicates whether the task has completed.
	 * @return if the task has completed (that is, the task is in one of the three
	 * final states: , , or ); otherwise, .
	 */
	var IsCompleted(default, never):Bool;
	var IsCompletedSuccessfully(default, never):Bool;
	/**
	 * Gets whether the  completed due to an unhandled exception.
	 * @return if the task has thrown an unhandled exception; otherwise .
	 */
	var IsFaulted(default, never):Bool;
	/**
	 * Gets the  of this task.
	 * @return The current  of this task instance.
	 */
	var Status(default, never):cs.system.threading.tasks.TaskStatus;
	@:overload(function(action:cs.system.Action):Void {})
	@:overload(function(action:cs.system.Action, cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(action:cs.system.Action, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic):Void {})
	@:overload(function(action:cs.system.Action, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void {})
	function new(action:cs.system.Action_1<Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions):Void;
	@:overload(function(millisecondsDelay:Int):cs.system.threading.tasks.Task {})
	@:overload(function(delay:cs.system.TimeSpan):cs.system.threading.tasks.Task {})
	@:overload(function(millisecondsDelay:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Creates a task that completes after a specified number of milliseconds.
	 * @param millisecondsDelay The number of milliseconds to wait before completing
	 * the returned task, or -1 to wait indefinitely.
	 * @return A task that represents the time delay.
	 */
	static function Delay(delay:cs.system.TimeSpan, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Creates a  that's completed due to cancellation with a specified cancellation
	 * token.
	 * @param cancellationToken The cancellation token with which to complete the task.
	 * @return The canceled task.
	 */
	static function FromCanceled<TResult>(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(exception:cs.system.Exception):cs.system.threading.tasks.Task {})
	/**
	 * Creates a  that has completed with a specified exception.
	 * @param exception The exception with which to complete the task.
	 * @return The faulted task.
	 */
	static function FromException<TResult>(exception:cs.system.Exception):cs.system.threading.tasks.Task_1<TResult>;
	/**
	 * Creates a  that's completed successfully with the specified result.
	 * @param TResult The type of the result returned by the task.
	 * @param result The result to store into the completed task.
	 * @return The successfully completed task.
	 */
	static function FromResult<TResult>(result:TResult):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(action:cs.system.Action):cs.system.threading.tasks.Task {})
	@:overload(function(function_:cs.system.Func_1<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_1<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(function_:cs.system.Func_1<TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(action:cs.system.Action, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(function_:cs.system.Func_1<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_1<cs.system.threading.tasks.Task_1<TResult>>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	/**
	 * Queues the specified work to run on the thread pool and returns a  object that
	 * represents that work.
	 * @param action The work to execute asynchronously
	 * @return A task that represents the work queued to execute in the ThreadPool.
	 */
	static function Run<TResult>(function_:cs.system.Func_1<TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>):Void {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, millisecondsTimeout:Int):Bool {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, timeout:cs.system.TimeSpan):Bool {})
	/**
	 * Waits for all of the provided  objects to complete execution.
	 * @param tasks An array of  instances on which to wait.
	 */
	static function WaitAll(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool;
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>):Int {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, millisecondsTimeout:Int):Int {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken):Int {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, timeout:cs.system.TimeSpan):Int {})
	/**
	 * Waits for any of the provided  objects to complete execution.
	 * @param tasks An array of  instances on which to wait.
	 * @return The index of the completed  object in the  array.
	 */
	static function WaitAny(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Int;
	@:overload(function(tasks:cs.system.collections.generic.IEnumerable<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.system.collections.generic.IEnumerable<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<cs.NativeArray<TResult>> {})
	/**
	 * Creates a task that will complete when all of the  objects in an enumerable
	 * collection have completed.
	 * @param tasks The tasks to wait on for completion.
	 * @return A task that represents the completion of all of the supplied tasks.
	 */
	static function WhenAll<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<cs.NativeArray<TResult>>;
	@:overload(function(tasks:cs.system.collections.generic.IEnumerable<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task> {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task> {})
	@:overload(function<TResult>(tasks:cs.system.collections.generic.IEnumerable<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task_1<TResult>> {})
	/**
	 * Creates a task that will complete when any of the supplied tasks have completed.
	 * @param tasks The tasks to wait on for completion.
	 * @return A task that represents the completion of one of the supplied tasks.  The
	 * return task's Result is the task that completed.
	 */
	static function WhenAny<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TResult>>):cs.system.threading.tasks.Task_1<cs.system.threading.tasks.Task_1<TResult>>;
	/**
	 * Creates an awaitable task that asynchronously yields back to the current context
	 * when awaited.
	 * @return A context that, when awaited, will asynchronously transition back into
	 * the current context at the time of the await. If the current  is non-null, it is
	 * treated as the current context. Otherwise, the task scheduler that is associated
	 * with the currently executing task is treated as the current context.
	 */
	static function Yield():cs.system.runtime.compilerservices.YieldAwaitable;
	/**
	 * Configures an awaiter used to await this .
	 * @param continueOnCapturedContext to attempt to marshal the continuation back to
	 * the original context captured; otherwise, .
	 * @return An object used to await this task.
	 */
	function ConfigureAwait(continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredTaskAwaitable;
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task, Dynamic>, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task, Dynamic, TResult>, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task, Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task, Dynamic>, state:Dynamic, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task, Dynamic>, state:Dynamic, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task, Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task, Dynamic, TResult>, state:Dynamic, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task, Dynamic, TResult>, state:Dynamic, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(continuationAction:cs.system.Action_2<cs.system.threading.tasks.Task, Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	/**
	 * Creates a continuation that receives caller-supplied state information and
	 * executes when the target  completes.
	 * @param continuationAction An action to run when the task completes. When run,
	 * the delegate is passed the completed task and a caller-supplied state object as
	 * arguments.
	 * @param state An object representing data to be used by the continuation action.
	 * @return A new continuation task.
	 */
	function ContinueWith<TResult>(continuationFunction:cs.system.Func_3<cs.system.threading.tasks.Task, Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult>;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Gets an awaiter used to await this .
	 * @return An awaiter instance.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.TaskAwaiter;
	@:overload(function():Void {})
	/** Runs the  synchronously on the current . */
	function RunSynchronously(scheduler:cs.system.threading.tasks.TaskScheduler):Void;
	@:overload(function():Void {})
	/** Starts the , scheduling it for execution to the current . */
	function Start(scheduler:cs.system.threading.tasks.TaskScheduler):Void;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	/** Waits for the  to complete execution. */
	function Wait(millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool;
}
