package haxe.atomic;

import java.util.concurrent.atomic.AtomicInteger;

abstract AtomicInt(AtomicInteger) {
	public inline function new(value:Int) {
		this = new AtomicInteger(value);
	}

	private inline function cas_loop(value:Int, op:(a:Int, b:Int) -> Int):Int {
		var val;

		do {
			val = this.get();
		} while (!this.compareAndSet(val, op(val, value)));

		return val;
	}

	public inline function add(b:Int):Int {
		return this.getAndAdd(b);
	}

	public inline function sub(b:Int):Int {
		return this.getAndAdd(-b);
	}

	public inline function and(b:Int):Int {
		return cas_loop(b, (a:Int, b:Int) -> a & b);
	}

	public inline function or(b:Int):Int {
		return cas_loop(b, (a:Int, b:Int) -> a | b);
	}

	public inline function xor(b:Int):Int {
		return cas_loop(b, (a:Int, b:Int) -> a ^ b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		// Java's compareAndSet returns a boolean, so do a CAS loop to be able to return the original value without a potential race condition

		var original;
		var real_replacement;
		do {
			original = this.get();
			real_replacement = original == expected ? replacement : original;
		} while (!this.compareAndSet(original, real_replacement));
		return original;
	}

	public inline function exchange(value:Int):Int {
		return this.getAndSet(value);
	}

	public inline function load():Int {
		return this.get();
	}

	public inline function store(value:Int):Int {
		this.set(value);
		return value;
	}
}
