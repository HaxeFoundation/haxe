package haxe.coro.schedulers;

import sys.thread.EventLoop;

class EventLoopScheduler implements IScheduler {
    final loop : EventLoop;

    public function new(loop) {
        this.loop = loop;
    }

    public function schedule(func : ()->Void) {
        loop.run(func);
    }

	public function scheduleIn(func : ()->Void, ms:Int) {
		var handle : EventHandler = null;

		handle = loop.repeat(() -> {
			loop.cancel(handle);

			func();
		}, ms);
	}
}