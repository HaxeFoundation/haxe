package cs.system.threading.tasks;

@:native("System.Threading.Tasks.TaskAsyncEnumerableExtensions")
extern class TaskAsyncEnumerableExtensions {
	@:overload(function(source:cs.system.IAsyncDisposable, continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredAsyncDisposable {})
	static function ConfigureAwait<T>(source:cs.system.collections.generic.IAsyncEnumerable<T>, continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredCancelableAsyncEnumerable<T>;
	static function WithCancellation<T>(source:cs.system.collections.generic.IAsyncEnumerable<T>, cancellationToken:cs.system.threading.CancellationToken):cs.system.runtime.compilerservices.ConfiguredCancelableAsyncEnumerable<T>;
}
