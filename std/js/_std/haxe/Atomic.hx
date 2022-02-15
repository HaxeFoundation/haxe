package haxe;

import js.lib.Atomics;

@:coreApi
@:using(haxe.Atomic)
extern abstract Atomic<T:Int>(js.lib.Int32Array) to js.lib.Int32Array {
	public inline function new(value:T) {
		this = new js.lib.Int32Array(new js.lib.SharedArrayBuffer(4));
		this[0] = value;
	}

	public extern static inline function add<T:Int>(a:Atomic<T>, b:T):T {
		return cast Atomics.add(a, 0, b);
	}

	public extern static inline function sub<T:Int>(a:Atomic<T>, b:T):T {
		return cast Atomics.sub(a, 0, b);
	}

	public extern static inline function and<T:Int>(a:Atomic<T>, b:T):T {
		return cast Atomics.and(a, 0, b);
	}

	public extern static inline function or<T:Int>(a:Atomic<T>, b:T):T {
		return cast Atomics.or(a, 0, b);
	}

	public extern static inline function xor<T:Int>(a:Atomic<T>, b:T):T {
		return cast Atomics.xor(a, 0, b);
	}

	public extern static inline function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T {
		return cast Atomics.compareExchange(a, 0, expected, replacement);
	}

	public extern static inline function exchange<T:Int>(a:Atomic<T>, value:T):T {
		return cast Atomics.exchange(a, 0, value);
	}

	public extern static inline function load<T:Int>(a:Atomic<T>):T {
		return cast Atomics.load(a, 0);
	}

	public extern static inline function store<T:Int>(a:Atomic<T>, value:T):T {
		return cast Atomics.store(a, 0, value);
	}
}
