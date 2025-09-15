package hxcoro.concurrent;

import haxe.coro.Mutex;

#if (cpp || hl || js || jvm || eval)
typedef AtomicInt = haxe.atomic.AtomicInt;
#else
typedef AtomicInt = AtomicIntImpl;

private class AtomicIntData {
	public final mutex:Mutex;
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
		mutex = new Mutex();
	}
}

abstract AtomicIntImpl(AtomicIntData) {
	public function new(v:Int) {
		this = new AtomicIntData(v);
	}

	public function load() {
		return this.value;
	}

	public function compareExchange(expected:Int, replacement:Int) {
		this.mutex.acquire();
		if (this.value == expected) {
			this.value = replacement;
			this.mutex.release();
			return expected;
		} else {
			final value = this.value;
			this.mutex.release();
			return value;
		}
	}

	public function sub(b:Int) {
		this.mutex.acquire();
		final value = this.value;
		this.value -= b;
		this.mutex.release();
		return value;
	}

	public function add(b:Int) {
		this.mutex.acquire();
		final value = this.value;
		this.value += b;
		this.mutex.release();
		return value;
	}

	public function store(b:Int) {
		this.mutex.acquire();
		final value = this.value;
		this.value = b;
		this.mutex.release();
		return value;
	}
}
#end
