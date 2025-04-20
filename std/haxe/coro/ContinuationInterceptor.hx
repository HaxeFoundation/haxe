package haxe.coro;

abstract class ContinuationInterceptor {
	public function new() { }

	public abstract function intercept<T>(continuation:IContinuation<T>):IContinuation<T>;

	public function release<T>(continuation:IContinuation<T>) {}
}