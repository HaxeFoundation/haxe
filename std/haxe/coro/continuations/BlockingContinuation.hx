package haxe.coro.continuations;

import sys.thread.EventLoop;

class BlockingContinuation implements IContinuation<Any> {
	public final _hx_context:CoroutineContext;

	final loop:EventLoop;

	var running:Bool;
	var result:Int;
	var error:Exception;

	public var _hx_recursing:Bool;

	public function new(loop, scheduler) {
		this.loop = loop;

		_hx_context = new CoroutineContext(scheduler);
		running = true;
		result = 0;
		error = null;
	}

	public function resume(result:Any, error:Exception) {
		running = false;

		this.result = result;
		this.error = error;
	}

	public function wait():Any {
		while (running) {
			switch loop.progress() {
				case Never:
					break;
				case _:
					continue;
			}
		}

		if (error != null) {
			throw error;
		} else {
			return cast result;
		}
	}
}
