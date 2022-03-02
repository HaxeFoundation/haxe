package haxe.atomic;

import java.util.concurrent.atomic.AtomicReference;

abstract AtomicObject<T:{}>(AtomicReference<T>) {
	public function new(value:T) {
		this = new AtomicReference(value);
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		final original = this.get();
		if (this.compareAndSet(expected, replacement)) {} // TODO: this is probably subject to race conditions and stuff
		return original;
	}

	public inline function exchange(value:T):T {
		return this.getAndSet(value);
	}

	public inline function load():T {
		return this.get();
	}

	public extern inline function store(value:T):T {
		this.set(value);
		return value;
	}
}
