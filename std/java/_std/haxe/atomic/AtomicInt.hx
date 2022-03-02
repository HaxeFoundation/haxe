package haxe.atomic;

import java.util.concurrent.atomic.AtomicInteger;

abstract AtomicInt(AtomicInteger) {
	public function new(value:Int) {
		this = new AtomicInteger(value);
	}

	private inline function cas_loop(value:Int, op:(a:Int, b:Int) -> Int):Int {
		var val = this.get();

		while (!this.compareAndSet(val, op(val, value))) {
			val = this.get();
		}

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
		final original = this.get();
		if (this.compareAndSet(expected, replacement)) {} // TODO: this is probably subject to race conditions and stuff
		return original;
	}

	public inline function exchange(value:Int):Int {
		return this.getAndSet(value);
	}

	public inline function load():Int {
		return this.get();
	}

	public extern inline function store(value:Int):Int {
		this.set(value);
		return value;
	}
}
