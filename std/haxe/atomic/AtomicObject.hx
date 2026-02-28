package haxe.atomic;

#if target.threaded

import sys.thread.Mutex;

private class AtomicObjectValue<T:{}> {
	public final mutex:Mutex;
	public var value:T;

	public function new(value:T) {
		this.value = value;
		mutex = new Mutex();
	}
}

private typedef AtomicObjectData<T:{}> = AtomicObjectValue<T>;

/**
	Atomic object. Use with care, this does not magically make it thread-safe to mutate objects.
**/
abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	public function new(value:T):Void {
		this = new AtomicObjectData(value);
	}

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.

		Note that comparison is done by reference, and not by value.
		While this is expected for most objects, this might give unexpected behaviour for strings.
	**/
	public function compareExchange(expected:T, replacement:T):T {
		this.mutex.acquire();
		final value = this.value;
		if (this.value == expected) {
			this.value = replacement;
		}
		this.mutex.release();
		return value;
	}

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:T):T {
		this.mutex.acquire();
		final current = this.value;
		this.value = value;
		this.mutex.release();
		return current;
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():T {
		return this.value;
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:T):T {
		this.mutex.acquire();
		this.value = value;
		this.mutex.release();
		return value;
	}
}

#else

private class AtomicObjectValue<T:{}> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}
}

private typedef AtomicObjectData<T:{}> = AtomicObjectValue<T>;

/**
	Atomic object. Use with care, this does not magically make it thread-safe to mutate objects.
	Not supported on JavaScript.
**/
abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	public function new(value:T):Void {
		this = new AtomicObjectData(value);
	}

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.

		Note that comparison is done by reference, and not by value.
		While this is expected for most objects, this might give unexpected behaviour for strings.
	**/
	public function compareExchange(expected:T, replacement:T):T {
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
	public function exchange(value:T):T {
		final current = this.value;
		this.value = value;
		return current;
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():T {
		return this.value;
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:T):T {
		this.value = value;
		return value;
	}
}

#end
