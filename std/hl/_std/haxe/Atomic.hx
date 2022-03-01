package haxe;

#if (hl_ver < version("1.12.0") && !doc_gen)
#error "Atomic operations require HL 1.12+"
#end

@:coreApi
@:using(haxe.Atomic)
abstract Atomic<T:Int>(hl.NativeArray<Int>) {
	public extern inline function new(value:T):Void {
		this = new hl.NativeArray(1);
		this[0] = value;
	}

	inline function getRef():hl.Ref<Int> {
		return this.getRef();
	}

	public extern inline static function add<T:Int>(a:Atomic<T>, b:T):T {
		return untyped $atomicAdd(a.getRef(), b);
	}

	public extern static inline function sub<T:Int>(a:Atomic<T>, b:T):T {
		return untyped $atomicSub(a.getRef(), b);
	}

	public extern static inline function and<T:Int>(a:Atomic<T>, b:T):T {
		return untyped $atomicAnd(a.getRef(), b);
	}

	public extern static inline function or<T:Int>(a:Atomic<T>, b:T):T {
		return untyped $atomicOr(a.getRef(), b);
	}

	public extern static inline function xor<T:Int>(a:Atomic<T>, b:T):T {
		return untyped $atomicXor(a.getRef(), b);
	}

	public extern static inline function compareExchange<T:Int>(a:Atomic<T>, expected:T, replacement:T):T {
		return untyped $atomicCompareExchange(a.getRef(), expected, replacement);
	}

	public extern static inline function exchange<T:Int>(a:Atomic<T>, value:T):T {
		return untyped $atomicExchange(a.getRef(), value);
	}

	// public extern static inline function isLockFree(byteSize:Int):Bool {
	// 	return untyped $atomicIsLockFree(byteSize);
	// }

	public extern static inline function load<T:Int>(a:Atomic<T>):T {
		return untyped $atomicLoad(a.getRef());
	}

	public extern static inline function store<T:Int>(a:Atomic<T>, value:T):T {
		return untyped $atomicStore(a.getRef(), value);
	}
}
