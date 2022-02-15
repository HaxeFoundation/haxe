package haxe;

import cs.system.threading.Interlocked.*;

private class Wrapper {
	public var value:Int;

	public inline function new(value:Int) {
		this.value = value;
	}
}

@:coreApi
@:using(haxe.Atomic)
abstract Atomic<T:Int>(Wrapper) {
	public extern inline function new(value:T) {
		this = new Wrapper(value);
	}

	private extern static inline function cas_loop(a:Wrapper, value:Int, op:(a:Int, b:Int) -> Int):Int {
		var val = (cast a:Wrapper).value;

		while(CompareExchange((cast a:Wrapper).value, op(val, value), val) != val) {
			val = (cast a:Wrapper).value;
		}
		return val;
	}

	public extern static inline function add<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop((cast a:Wrapper), b, (a,b) -> a + b);
	}

	public extern static inline function sub<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop((cast a:Wrapper), b, (a,b) -> a - b);
	}

	public extern static inline function and<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop((cast a:Wrapper), b, (a,b) -> a & b);
	}
	
	public extern static inline function or<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop((cast a:Wrapper), b, (a,b) -> a | b);
	}
	
	public extern static inline function xor<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop((cast a:Wrapper), b, (a,b) -> a ^ b);
	}

	public extern static inline function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T {
		return cast CompareExchange((cast a:Wrapper).value, (replacement:Int), (expected:Int));
	}

	public extern static inline function exchange<T:Int>(a:Atomic<T>, value:T):T {
		return cast Exchange((cast a:Wrapper).value, (value:Int));
	}

	public extern static inline function load<T:Int>(a:Atomic<T>):T {
		return cast (cast a:Wrapper).value;
	}

	public extern static inline function store<T:Int>(a:Atomic<T>, value:T):T {
		return cast (cast a:Wrapper).value = cast value; // according to the CLI spec reads and writes are atomic
	}
}
