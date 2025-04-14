package haxe.coro;

import sys.thread.EventLoop;

private typedef EventLoopImpl = sys.thread.EventLoop;

@:coreApi abstract EventLoop(EventLoopImpl) {
    public function new() {
        this = new EventLoopImpl();
    }

    public function tick():Bool {
        return switch this.progress() {
            case Never:
                false;
            case _:
                true;
        }
    }

    public function run(func:()->Void):Void {
        this.run(func);
    }

    public function runIn(func:()->Void, ms:Int):Void {
        var handle : EventHandler = null;

		handle = this.repeat(() -> {
			this.cancel(handle);

			func();
		}, ms);
    }
}