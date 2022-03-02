package haxe.atomic;

import cs.system.threading.Interlocked.*;

private class ObjectWrapper<T:{}> {
	public var value:T;

	public inline function new(value:T) {
		this.value = value;
	}
}

abstract AtomicObject<T:{}>(ObjectWrapper<T>) {
	public inline function new(value:T) {
		this = new ObjectWrapper(value);
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		return CompareExchange(this.value, replacement, expected);
	}

	public inline function exchange(value:T):T {
		return Exchange(this.value, value);
	}

	public inline function load():T {
		return this.value;
	}

	public inline function store(value:T):T {
		return this.value = value; // according to the CLI spec reads and writes are atomic
	}
}
