package haxe.atomic;

#if (hl_ver < version("1.12.0") && !doc_gen)
#error "Atomic operations require HL 1.12+"
#end

abstract AtomicInt(hl.NativeArray<Int>) {
	public inline function new(value:Int):Void {
		this = new hl.NativeArray(1);
		this[0] = value;
	}

	public inline function add(b:Int):Int {
		return untyped $atomicAdd(this.getRef(), b);
	}

	public inline function sub(b:Int):Int {
		return untyped $atomicSub(this.getRef(), b);
	}

	public inline function and(b:Int):Int {
		return untyped $atomicAnd(this.getRef(), b);
	}

	public inline function or(b:Int):Int {
		return untyped $atomicOr(this.getRef(), b);
	}

	public inline function xor(b:Int):Int {
		return untyped $atomicXor(this.getRef(), b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		return untyped $atomicCompareExchange(this.getRef(), expected, replacement);
	}

	public inline function exchange(value:Int):Int {
		return untyped $atomicExchange(this.getRef(), value);
	}

	public inline function load():Int {
		return untyped $atomicLoad(this.getRef());
	}

	public inline function store(value:Int):Int {
		return untyped $atomicStore(this.getRef(), value);
	}
}
