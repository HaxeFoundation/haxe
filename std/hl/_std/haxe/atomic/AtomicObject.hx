package haxe.atomic;

#if (hl_ver >= version("1.13.0"))
import hl.Atomics;

// use hl.NativeArray<Dynamic> instead of hl.NativeArray<T>
// so that the compiler doesn't get confused and emit hl.Ref.make(this.getRef())
private typedef AtomicObjectData<T:{}> = hl.NativeArray<Dynamic>;

abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
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
#else
// HL < 1.13 has no native atomics: fall back to a mutex
import sys.thread.Mutex;

private class AtomicObjectValue<T:{}> {
	public final mutex:Mutex;
	public var value:T;

	public function new(value:T) {
		this.value = value;
		mutex = new Mutex();
	}
}

private typedef AtomicObjectData<T:{}> = AtomicObjectValue<T>;

abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	public function new(value:T):Void {
		this = new AtomicObjectData(value);
	}

	public function compareExchange(expected:T, replacement:T):T {
		this.mutex.acquire();
		final value = this.value;
		if (value == expected)
			this.value = replacement;
		this.mutex.release();
		return value;
	}

	public function exchange(value:T):T {
		this.mutex.acquire();
		final current = this.value;
		this.value = value;
		this.mutex.release();
		return current;
	}

	public function load():T {
		this.mutex.acquire();
		final value = this.value;
		this.mutex.release();
		return value;
	}

	public function store(value:T):T {
		this.mutex.acquire();
		this.value = value;
		this.mutex.release();
		return value;
	}
}
#end
