package haxe.coro;

class CoroutineContext {
    public final scheduler : IScheduler;
	public var interceptor : Null<ContinuationInterceptor>;

    public function new(scheduler) {
        this.scheduler = scheduler;
    }

	public function maybeIntercept<T>(continuation:IContinuation<T>, result:Null<T>, error:Null<Exception>) {
		if (interceptor != null) {
			final cont = interceptor.intercept(continuation);
			cont.resume(result, error);
			if (cont != continuation) {
				interceptor.release(cont);
			}
		} else {
			continuation.resume(result, error);
		}
	}
}