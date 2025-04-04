package haxe.coro;

class CoroutineContext {
    public final scheduler : IScheduler;

    public function new(scheduler) {
        this.scheduler = scheduler;
    }
}