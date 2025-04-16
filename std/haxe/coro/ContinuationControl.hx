package haxe.coro;

enum abstract ContinuationControl(Int) {
	final Pending;
	final Returned;
	final Thrown;
}