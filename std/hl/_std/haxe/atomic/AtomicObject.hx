package haxe.atomic;

#if (hl_ver < version("1.12.0") && !doc_gen)
#error "Atomic operations require HL 1.12+"
#end

abstract AtomicObject<T:{}>(hl.NativeArray<T>) {
	public inline function new(value:T):Void {
		this = new hl.NativeArray(1);
		this[0] = value;
	}

	public inline function compareExchange(expected:T, replacement:T):T {
		return untyped $atomicCompareExchange(this.getRef(), expected, replacement);
	}

	public inline function exchange(value:T):T {
		return untyped $atomicExchange(this.getRef(), value);
	}

	public inline function load():T {
		return untyped $atomicLoad(this.getRef());
	}

	public inline function store(value:T):T {
		return untyped $atomicStore(this.getRef(), value);
	}
}
