package haxe.atomic;

#if (hl_ver < version("1.13.0") && !doc_gen)
#error "Atomic operations require HL 1.13+"
#end
import hl.Atomics;

#if doc_gen
@:coreApi
@:coreType
abstract AtomicInt {
	public function new(value:Int):Void;

	public function add(b:Int):Int;

	public function sub(b:Int):Int;

	public function and(b:Int):Int;

	public function or(b:Int):Int;

	public function xor(b:Int):Int;

	public function compareExchange(expected:Int, replacement:Int):Int;

	public function exchange(value:Int):Int;

	public function load():Int;

	public function store(value:Int):Int;
}
#else
abstract AtomicInt(hl.NativeArray<Int>) {
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
#end
