package haxe.atomic;

#if target.threaded

import sys.thread.Mutex;

private class AtomicIntValue {
	public final mutex:Mutex;
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
		mutex = new Mutex();
	}
}

private typedef AtomicIntData = AtomicIntValue;

/**
	Atomic integer.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
abstract AtomicInt(AtomicIntData) {
	public function new(value:Int):Void {
		this = new AtomicIntData(value);
	}

	/**
		Atomically adds `b` to `a`.
		Returns the original value of `a`.
	**/
	public function add(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value += b;
		this.mutex.release();
		return value;
	}

	/**
		Atomically subtracts `b` from `a`.
		Returns the original value of `a`.
	**/
	public function sub(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value -= b;
		this.mutex.release();
		return value;
	}

	/**
		Atomically computes the bitwise and of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function and(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value &= b;
		this.mutex.release();
		return value;
	}

	/**
		Atomically computes the bitwise or of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function or(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value |= b;
		this.mutex.release();
		return value;
	}

	/**
		Atomically computes the bitwise xor of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function xor(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value ^= b;
		this.mutex.release();
		return value;
	}

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:Int, replacement:Int):Int {
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

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:Int):Int {
		this.mutex.acquire();
		final current = this.value;
		this.value = value;
		this.mutex.release();
		return current;
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():Int {
		return this.value;
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:Int):Int {
		this.mutex.acquire();
		this.value = value;
		this.mutex.release();
		return value;
	}
}

#else

private class AtomicIntValue {
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
	}
}

private typedef AtomicIntData = AtomicIntValue;

/**
	Atomic integer.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
abstract AtomicInt(AtomicIntData) {
	public function new(value:Int):Void {
		this = new AtomicIntData(value);
	}

	/**
		Atomically adds `b` to `a`.
		Returns the original value of `a`.
	**/
	public function add(b:Int):Int {
		final value = this.value;
		this.value += b;
		return value;
	}

	/**
		Atomically subtracts `b` from `a`.
		Returns the original value of `a`.
	**/
	public function sub(b:Int):Int {
		final value = this.value;
		this.value -= b;
		return value;
	}

	/**
		Atomically computes the bitwise and of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function and(b:Int):Int {
		final value = this.value;
		this.value &= b;
		return value;
	}

	/**
		Atomically computes the bitwise or of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function or(b:Int):Int {
		final value = this.value;
		this.value |= b;
		return value;
	}

	/**
		Atomically computes the bitwise xor of `a` and `b` and stores it in `a`.
		Returns the original value of `a`.
	**/
	public function xor(b:Int):Int {
		final value = this.value;
		this.value ^= b;
		return value;
	}

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:Int, replacement:Int):Int {
		final value = this.value;
		if (this.value == expected) {
			this.value = replacement;
		}
		return value;
	}

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:Int):Int {
		final current = this.value;
		this.value = value;
		return current;
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():Int {
		return this.value;
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:Int):Int {
		this.value = value;
		return value;
	}
}

#end
