package haxe.coro;

#if (target.threaded && !cppia && !eval)
import sys.thread.EventLoop;
private typedef EventLoopImpl = sys.thread.EventLoop;
#else
import haxe.coro.EventLoopImpl;
private typedef EventLoopImpl = haxe.coro.EventLoopImpl;
#end

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