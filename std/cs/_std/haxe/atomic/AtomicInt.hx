package haxe.atomic;

import cs.system.threading.Interlocked.*;

private class IntWrapper {
	public var value:Int;

	public inline function new(value:Int) {
		this.value = value;
	}
}

abstract AtomicInt(IntWrapper) to IntWrapper {
	public inline function new(value:Int) {
		this = new IntWrapper(value);
	}

	private inline function cas_loop(value:Int, op:(a:Int, b:Int) -> Int):Int {
		var val = this.value;

		while (CompareExchange(this.value, op(val, value), val) != val) {
			val = this.value;
		}
		return val;
	}

	public inline function add(b:Int):Int {
		return cas_loop(b, (a, b) -> a + b);
	}

	public inline function sub(b:Int):Int {
		return cas_loop(b, (a, b) -> a - b);
	}

	public inline function and(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a & b);
	}

	public inline function or(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a | b);
	}

	public inline function xor(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a ^ b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		return CompareExchange(this.value, replacement, expected);
	}

	public inline function exchange(value:Int):Int {
		return Exchange(this.value, value);
	}

	public inline function load():Int {
		return this.value;
	}

	public inline function store(value:Int):Int {
		return this.value = value; // according to the CLI spec reads and writes are atomic
	}
}
