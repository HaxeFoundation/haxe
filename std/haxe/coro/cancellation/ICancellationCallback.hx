package haxe.coro.cancellation;

import haxe.exceptions.CancellationException;

interface ICancellationCallback {
	function onCancellation(cause:CancellationException):Void;
}
