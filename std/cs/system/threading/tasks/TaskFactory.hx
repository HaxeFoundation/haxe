package cs.system.threading.tasks;

/** Provides support for creating and scheduling  objects. */
@:native("System.Threading.Tasks.TaskFactory")
extern class TaskFactory {
	/**
	 * Gets the default cancellation token for this task factory.
	 * @return The default task cancellation token for this task factory.
	 */
	var CancellationToken(default, never):cs.system.threading.CancellationToken;
	/**
	 * Gets the default task continuation options for this task factory.
	 * @return The default task continuation options for this task factory.
	 */
	var ContinuationOptions(default, never):cs.system.threading.tasks.TaskContinuationOptions;
	/**
	 * Gets the default task creation options for this task factory.
	 * @return The default task creation options for this task factory.
	 */
	var CreationOptions(default, never):cs.system.threading.tasks.TaskCreationOptions;
	/**
	 * Gets the default task scheduler for this task factory.
	 * @return The default task scheduler for this task factory.
	 */
	var Scheduler(default, never):cs.system.threading.tasks.TaskScheduler;
	@:overload(function():Void {})
	@:overload(function(cancellationToken:cs.system.threading.CancellationToken):Void {})
	@:overload(function(scheduler:cs.system.threading.tasks.TaskScheduler):Void {})
	@:overload(function(creationOptions:cs.system.threading.tasks.TaskCreationOptions, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):Void {})
	function new(cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):Void;
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task>>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task>, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>>):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task>>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task>>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task>, TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task>, TResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, TResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task>>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task>, TResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	/**
	 * Creates a continuation task that starts when a set of specified tasks has
	 * completed.
	 * @param tasks The array of tasks from which to continue.
	 * @param continuationAction The action delegate to execute when all tasks in the 
	 * array have completed.
	 * @return The new continuation task.
	 */
	function ContinueWhenAll<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, TResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TAntecedentResult>>):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TAntecedentResult>, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TAntecedentResult>>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TAntecedentResult>, TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TAntecedentResult>, TResult>, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task, TResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TAntecedentResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationAction:cs.system.Action_1<cs.system.threading.tasks.Task_1<TAntecedentResult>>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	/**
	 * Creates a continuation  that will be started upon the completion of any Task in
	 * the provided set.
	 * @param tasks The array of tasks from which to continue when one task completes.
	 * @param continuationAction The action delegate to execute when one task in the 
	 * array completes.
	 * @return The new continuation .
	 */
	function ContinueWhenAny<TAntecedentResult, TResult>(tasks:cs.NativeArray<cs.system.threading.tasks.Task_1<TAntecedentResult>>, continuationFunction:cs.system.Func_2<cs.system.threading.tasks.Task_1<TAntecedentResult>, TResult>, cancellationToken:cs.system.threading.CancellationToken, continuationOptions:cs.system.threading.tasks.TaskContinuationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Action_1<cs.system.IAsyncResult>):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(beginMethod:cs.system.Func_3<cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(beginMethod:cs.system.Func_3<cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(beginMethod:cs.system.Func_3<cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(beginMethod:cs.system.Func_3<cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1>(beginMethod:cs.system.Func_4<TArg1, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(asyncResult:cs.system.IAsyncResult, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1, TResult>(beginMethod:cs.system.Func_4<TArg1, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1>(beginMethod:cs.system.Func_4<TArg1, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TArg1, TResult>(beginMethod:cs.system.Func_4<TArg1, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1, TArg2>(beginMethod:cs.system.Func_5<TArg1, TArg2, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, arg2:TArg2, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function<TArg1, TArg2, TResult>(beginMethod:cs.system.Func_5<TArg1, TArg2, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, arg2:TArg2, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1, TArg2>(beginMethod:cs.system.Func_5<TArg1, TArg2, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, arg2:TArg2, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TArg1, TArg2, TResult>(beginMethod:cs.system.Func_5<TArg1, TArg2, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, arg2:TArg2, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1, TArg2, TArg3>(beginMethod:cs.system.Func_6<TArg1, TArg2, TArg3, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, arg2:TArg2, arg3:TArg3, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function<TArg1, TArg2, TArg3, TResult>(beginMethod:cs.system.Func_6<TArg1, TArg2, TArg3, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, arg2:TArg2, arg3:TArg3, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TArg1, TArg2, TArg3>(beginMethod:cs.system.Func_6<TArg1, TArg2, TArg3, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Action_1<cs.system.IAsyncResult>, arg1:TArg1, arg2:TArg2, arg3:TArg3, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	/**
	 * Creates a  that represents a pair of begin and end methods that conform to the
	 * Asynchronous Programming Model pattern.
	 * @param beginMethod The delegate that begins the asynchronous operation.
	 * @param endMethod The delegate that ends the asynchronous operation.
	 * @param state An object containing data to be used by the  delegate.
	 * @return The created  that represents the asynchronous operation.
	 */
	function FromAsync<TArg1, TArg2, TArg3, TResult>(beginMethod:cs.system.Func_6<TArg1, TArg2, TArg3, cs.system.AsyncCallback, Dynamic, cs.system.IAsyncResult>, endMethod:cs.system.Func_2<cs.system.IAsyncResult, TResult>, arg1:TArg1, arg2:TArg2, arg3:TArg3, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult>;
	@:overload(function(action:cs.system.Action):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_1<TResult>):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(action:cs.system.Action, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(action:cs.system.Action, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(function_:cs.system.Func_1<TResult>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(function_:cs.system.Func_1<TResult>, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function<TResult>(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, creationOptions:cs.system.threading.tasks.TaskCreationOptions):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(action:cs.system.Action, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	@:overload(function<TResult>(function_:cs.system.Func_1<TResult>, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult> {})
	@:overload(function(action:cs.system.Action_1<Dynamic>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task {})
	/**
	 * Creates and starts a task.
	 * @param action The action delegate to execute asynchronously.
	 * @return The started task.
	 */
	function StartNew<TResult>(function_:cs.system.Func_2<Dynamic, TResult>, state:Dynamic, cancellationToken:cs.system.threading.CancellationToken, creationOptions:cs.system.threading.tasks.TaskCreationOptions, scheduler:cs.system.threading.tasks.TaskScheduler):cs.system.threading.tasks.Task_1<TResult>;
}
