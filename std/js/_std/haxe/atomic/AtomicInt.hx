package haxe.atomic;

import js.lib.Atomics;

abstract AtomicInt(js.lib.Int32Array) to js.lib.Int32Array {
	public inline function new(value:Int) {
		this = new js.lib.Int32Array(new js.lib.SharedArrayBuffer(js.lib.Int32Array.BYTES_PER_ELEMENT));
		this[0] = value;
	}

	public inline function add(b:Int):Int {
		return Atomics.add(this, 0, b);
	}

	public inline function sub(b:Int):Int {
		return Atomics.sub(this, 0, b);
	}

	public inline function and(b:Int):Int {
		return Atomics.and(this, 0, b);
	}

	public inline function or(b:Int):Int {
		return Atomics.or(this, 0, b);
	}

	public inline function xor(b:Int):Int {
		return Atomics.xor(this, 0, b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		return Atomics.compareExchange(this, 0, expected, replacement);
	}

	public inline function exchange(value:Int):Int {
		return Atomics.exchange(this, 0, value);
	}

	public inline function load():Int {
		return Atomics.load(this, 0);
	}

	public inline function store(value:Int):Int {
		return Atomics.store(this, 0, value);
	}
}
