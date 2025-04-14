package haxe.coro.schedulers;

import haxe.coro.EventLoop;

class EventLoopScheduler implements IScheduler {
    final loop : EventLoop;

    public function new(loop) {
        this.loop = loop;
    }

    public function schedule(func : ()->Void) {
        loop.run(func);
    }

	public function scheduleIn(func : ()->Void, ms:Int) {
		loop.runIn(func, ms);
	}
}