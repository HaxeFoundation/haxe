package cs.system.collections.generic;

@:native("System.Collections.Generic.IAsyncEnumerator")
extern interface IAsyncEnumerator<T> extends cs.system.IAsyncDisposable {
	var Current(default, never):T;
	function MoveNextAsync():cs.system.threading.tasks.ValueTask_1<Bool>;
}
