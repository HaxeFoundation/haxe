package haxe.atomic;

#if (hl_ver < version("1.13.0") && !doc_gen)
#error "Atomic operations require HL 1.13+"
#end
import hl.Atomics;

// use hl.NativeArray<Dynamic> instead of hl.NativeArray<T>
// so that the compiler doesn't get confused and emit hl.Ref.make(this.getRef())
abstract AtomicObject<T:{}>(hl.NativeArray<Dynamic>) {
	public inline function new(value:T):Void {
		this = new hl.NativeArray(1);
		this[0] = value;
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		return Atomics.compareExchangePtr(this.getRef(), expected, replacement);
	}

	public inline function exchange(value:T):T {
		return Atomics.exchangePtr(this.getRef(), value);
	}

	public inline function load():T {
		return Atomics.loadPtr(this.getRef());
	}

	public inline function store(value:T):T {
		return Atomics.storePtr(this.getRef(), value);
	}
}
