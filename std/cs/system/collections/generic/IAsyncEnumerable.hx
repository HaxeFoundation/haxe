package cs.system.collections.generic;

@:native("System.Collections.Generic.IAsyncEnumerable")
extern interface IAsyncEnumerable<T> {
	function GetAsyncEnumerator(?cancellationToken:cs.system.threading.CancellationToken):cs.system.collections.generic.IAsyncEnumerator<T>;
}
