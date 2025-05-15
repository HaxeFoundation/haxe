package haxe.coro.schedulers;

import haxe.coro.EventLoop;

class EventLoopScheduler extends Scheduler {

    final loop : EventLoop;

    public function new(loop:EventLoop) {
		super();
        this.loop = loop;
    }

    public function schedule(func : ()->Void) {
        loop.run(func);
    }

	public function scheduleIn(func : ()->Void, ms:Int) {
		loop.runIn(func, ms);
	}

	public function toString() {
		return '[EventLoopScheduler: $loop]';
	}
}