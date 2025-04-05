package haxe.atomic;

import java.util.concurrent.atomic.AtomicBoolean;

abstract AtomicBool(AtomicBoolean) {
	public inline function new(value:Bool) {
		this = new AtomicBoolean(value);
	}

	public inline function compareExchange(expected:Bool, replacement:Bool):Bool {
		// Java's compareAndSet returns a boolean, so do a CAS loop to be able to return the original value without a potential race condition

		var original;
		var real_replacement;
		do {
			original = this.get();
			real_replacement = original == expected ? replacement : original;
		} while (!this.compareAndSet(original, real_replacement));
		return original;
	}

	public inline function exchange(value:Bool):Bool {
		return this.getAndSet(value);
	}

	public inline function load():Bool {
		return this.get();
	}

	public inline function store(value:Bool):Bool {
		this.set(value);
		return value;
	}
}
