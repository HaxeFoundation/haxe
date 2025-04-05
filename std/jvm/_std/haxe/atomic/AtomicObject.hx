package haxe.atomic;

import java.util.concurrent.atomic.AtomicReference;

abstract AtomicObject<T:{}>(AtomicReference<T>) {
	public inline function new(value:T) {
		this = new AtomicReference(value);
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		// Java's compareAndSet returns a boolean, so do a CAS loop to be able to return the original value without a potential race condition

		var original;
		var real_replacement;
		do {
			original = this.get();
			real_replacement = original == expected ? replacement : original;
		} while (!this.compareAndSet(original, real_replacement));
		return original;
	}

	public inline function exchange(value:T):T {
		return this.getAndSet(value);
	}

	public inline function load():T {
		return this.get();
	}

	public inline function store(value:T):T {
		this.set(value);
		return value;
	}
}
