package haxe.coro.continuations;

import hxcoro.concurrent.AtomicInt;
import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.schedulers.IScheduleObject;

private enum abstract State(Int) to Int {
	var Active;
	var Resumed;
	var Resolved;
}

class RacingContinuation<T> extends SuspensionResult<T> implements IContinuation<T> implements IScheduleObject {
	final inputCont:IContinuation<T>;

	var resumeState:AtomicInt;

	public var context(get, never):Context;

	final scheduler:Scheduler;

	public function new(inputCont:IContinuation<T>) {
		this.inputCont = inputCont;
		resumeState = new AtomicInt(Active);
		scheduler = context.get(Scheduler);
	}

	inline function get_context() {
		return inputCont.context;
	}

	public function resume(result:T, error:Exception):Void {
		this.result = result;
		this.error = error;
		if (resumeState.compareExchange(Active, Resumed) != Active) {
			scheduler.scheduleObject(this);
		}
	}

	public function resolve():Void {
		if (resumeState.compareExchange(Active, Resolved) == Active) {
			state = Pending;
		} else {
			if (error != null) {
				state = Thrown;
			} else {
				state = Returned;
			}
		}
	}

	public function onSchedule() {
		inputCont.resume(result, error);
	}
}
