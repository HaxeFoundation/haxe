package haxe.atomic;

#if (hl_ver >= version("1.13.0"))
import hl.Atomics;

private typedef AtomicIntData = hl.NativeArray<Int>;

abstract AtomicInt(AtomicIntData) {
	public inline function new(value:Int):Void {
		this = new hl.NativeArray(1);
		this[0] = value;
	}

	public inline function add(b:Int):Int {
		return Atomics.add32(this.getRef(), b);
	}

	public inline function sub(b:Int):Int {
		return Atomics.sub32(this.getRef(), b);
	}

	public inline function and(b:Int):Int {
		return Atomics.and32(this.getRef(), b);
	}

	public inline function or(b:Int):Int {
		return Atomics.or32(this.getRef(), b);
	}

	public inline function xor(b:Int):Int {
		return Atomics.xor32(this.getRef(), b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		return Atomics.compareExchange32(this.getRef(), expected, replacement);
	}

	public inline function exchange(value:Int):Int {
		return Atomics.exchange32(this.getRef(), value);
	}

	public inline function load():Int {
		return Atomics.load32(this.getRef());
	}

	public inline function store(value:Int):Int {
		return Atomics.store32(this.getRef(), value);
	}
}
#else
// HL < 1.13 has no native atomics: fall back to a mutex
import sys.thread.Mutex;

private class AtomicIntValue {
	public final mutex:Mutex;
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
		mutex = new Mutex();
	}
}

private typedef AtomicIntData = AtomicIntValue;

abstract AtomicInt(AtomicIntData) {
	public function new(value:Int):Void {
		this = new AtomicIntData(value);
	}

	public function add(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value += b;
		this.mutex.release();
		return value;
	}

	public function sub(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value -= b;
		this.mutex.release();
		return value;
	}

	public function and(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value &= b;
		this.mutex.release();
		return value;
	}

	public function or(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value |= b;
		this.mutex.release();
		return value;
	}

	public function xor(b:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		this.value ^= b;
		this.mutex.release();
		return value;
	}

	public function compareExchange(expected:Int, replacement:Int):Int {
		this.mutex.acquire();
		final value = this.value;
		if (value == expected)
			this.value = replacement;
		this.mutex.release();
		return value;
	}

	public function exchange(value:Int):Int {
		this.mutex.acquire();
		final current = this.value;
		this.value = value;
		this.mutex.release();
		return current;
	}

	public function load():Int {
		this.mutex.acquire();
		final value = this.value;
		this.mutex.release();
		return value;
	}

	public function store(value:Int):Int {
		this.mutex.acquire();
		this.value = value;
		this.mutex.release();
		return value;
	}
}
#end
