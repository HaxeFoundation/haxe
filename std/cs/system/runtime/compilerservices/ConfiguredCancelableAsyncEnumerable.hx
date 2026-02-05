package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConfiguredCancelableAsyncEnumerable")
extern class ConfiguredCancelableAsyncEnumerable<T> extends cs.system.ValueType {
	function ConfigureAwait(continueOnCapturedContext:Bool):cs.system.runtime.compilerservices.ConfiguredCancelableAsyncEnumerable<T>;
	function GetAsyncEnumerator():cs.system.runtime.compilerservices.ConfiguredCancelableAsyncEnumerable_Enumerator<T>;
	function WithCancellation(cancellationToken:cs.system.threading.CancellationToken):cs.system.runtime.compilerservices.ConfiguredCancelableAsyncEnumerable<T>;
}
