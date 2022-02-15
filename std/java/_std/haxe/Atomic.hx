package haxe;

import java.util.concurrent.atomic.AtomicInteger;

@:coreApi
@:using(haxe.Atomic)
abstract Atomic<T:Int>(AtomicInteger) {
	public extern inline function new(value:T) {
		this = new AtomicInteger(value);
	}

	private extern inline function native():AtomicInteger {
		return this;
	}

	private extern static inline function cas_loop(a:AtomicInteger, value:Int, op:(a:Int, b:Int) -> Int):Int {
		var val = a.get();

		while (!a.compareAndSet(val, op(val, value))) {
			val = a.get();
		}

		return val;
	}

	public extern static inline function add<T:Int>(a:Atomic<T>, b:T):T {
		return cast a.native().getAndAdd(b);
	}

	public extern static inline function sub<T:Int>(a:Atomic<T>, b:T):T {
		return cast a.native().getAndAdd(-b);
	}

	public extern static inline function and<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop(a.native(), cast(b,Int), (a:Int, b:Int) -> a & b);
	}
	
	public extern static inline function or<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop(a.native(), cast(b,Int), (a:Int, b:Int) -> a | b);
	}
	
	public extern static inline function xor<T:Int>(a:Atomic<T>, b:T):T {
		return cast cas_loop(a.native(), cast(b,Int), (a:Int, b:Int) -> a ^ b);
	}

	public extern static inline function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T {
		final original = a.native().get();
		if(a.native().compareAndSet(expected, replacement)) {
			
		}

		return cast original;
	}

	public extern static inline function exchange<T:Int>(a:Atomic<T>, value:T):T {
		return cast a.native().getAndSet(value);
	}

	public extern static inline function load<T:Int>(a:Atomic<T>):T {
		return cast a.native().get();
	}

	public extern static inline function store<T:Int>(a:Atomic<T>, value:T):T {
		a.native().set(value);
		return value;
	}
}
