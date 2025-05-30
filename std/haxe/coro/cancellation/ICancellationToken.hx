package haxe.coro.cancellation;

interface ICancellationToken {
	var isCancellationRequested (get, never) : Bool;

	function onCancellationRequested(func : ()->Void) : ICancellationHandle;
}
