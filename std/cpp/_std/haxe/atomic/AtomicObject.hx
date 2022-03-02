package haxe.atomic;

#error "TODO: implement AtomicObject for C++"

#if cppia
#error "Cppia does not support atomic operations yet."
#end

extern abstract AtomicObject<T>(cpp.Pointer<Dynamic>) {
	public extern inline function new(value:T) {
		this = cpp.Pointer.ofArray([]);
	}

	@:native("_hx_atomic_compare_exchange")
	public extern function compareExchange(expected:T, replacement:T):T;

	@:native("_hx_atomic_exchange")
	public extern function exchange(value:T):T;

	@:native("_hx_atomic_load")
	public extern function load():T;

	@:native("_hx_atomic_store")
	public extern function store(value:T):T;
}
