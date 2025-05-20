package haxe.coro.continuations;

import haxe.CallStack;
import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;

class BlockingContinuation<T> implements IContinuation<T> {
	public final context:Context;

	final loop:EventLoop;

	var running:Bool;
	var result:T;
	var error:Exception;

	public function new(loop:EventLoop, scheduler:Scheduler) {
		this.loop = loop;

		context = Context.empty();
		context.set(Scheduler.key, scheduler);
		running = true;
		error = null;
	}

	public function resume(result:T, error:Exception) {
		running = false;

		this.result = result;
		this.error = error;
	}

	public function wait():T {
		while (loop.tick() || running) {
			// Busy wait
		}

		if (error != null) {
			throw error;
		} else {
			return result;
		}
	}
}
