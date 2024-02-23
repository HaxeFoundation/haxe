package yield;
import haxe.coro.Coroutine;

typedef Yield<T> = Coroutine<T->Void>;

function sequence<T>(f:Coroutine<Yield<T>->Void>):Iterator<T> {
	var finished = false;
	var nextValue:T = null;

	var nextStep = null;

	function finish(_, err) {
		if (err != null) {
			throw err;
		}
		finished = true;
	}

	@:coroutine function yield(value:T) {
		nextValue = value;
		Coroutine.suspend(cont -> nextStep = cont);
	}

	function hasNext():Bool {
		if (nextStep == null) {
			nextStep = f.create(yield, finish);
			nextStep(null, Normal);
		}
		return !finished;
	}

	function next():T {
		var value = nextValue;
		nextStep(null, Normal);
		return value;
	}

	return {hasNext: hasNext, next: next};
}
