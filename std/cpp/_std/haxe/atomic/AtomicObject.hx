package haxe.atomic;

private typedef AtomicObjectData<T:{}> = Dynamic;

#if cppia
extern
#end
abstract AtomicObject<T: {}>(AtomicObjectData<T>) {
	#if cppia
	public function new(value:T):Void;

	public function compareExchange(expected:T, replacement:T):T;

	public function exchange(value:T):T;

	public function load():T;

	public function store(value:T):T;
	#else
	public #if !scriptable inline #end function new(value:T) {
		this = untyped __global__.__hxcpp_atomic_object_create(value);
	}

	public #if !scriptable inline #end function compareExchange(expected:T, replacement:T):T {
		return untyped __global__.__hxcpp_atomic_object_compare_exchange(this, expected, replacement);
	}

	public #if !scriptable inline #end function exchange(value:T):T {
		return untyped __global__.__hxcpp_atomic_object_exchange(this, value);
	}

	public #if !scriptable inline #end function load():T {
		return untyped __global__.__hxcpp_atomic_object_load(this);
	}

	public #if !scriptable inline #end function store(value:T):T {
		return untyped __global__.__hxcpp_atomic_object_store(this, value);
	}
	#end
}