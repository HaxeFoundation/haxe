package haxe.atomic;

@:include("hx/atomic/AtomicObject.hpp")
@:cpp.ManagedType({type: "AtomicObject", namespace: ["hx", "atomic"], flags: [StandardNaming]})
private extern class AtomicObjectImpl {
	function new(value:Dynamic);

	function load():Dynamic;
	function store(value:Dynamic):Dynamic;
	function exchange(value:Dynamic):Dynamic;
	function compareExchange(expected:Dynamic, replacement:Dynamic):Dynamic;
}

private typedef AtomicObjectData<T:{}> = AtomicObjectImpl;

#if cppia
extern
#end
abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	#if cppia
	public function new(value:T):Void;

	public function compareExchange(expected:T, replacement:T):T;

	public function exchange(value:T):T;

	public function load():T;

	public function store(value:T):T;
	#else
	public #if !scriptable inline #end function new(value:T):Void {
		this = new AtomicObjectImpl(value);
	}

	public #if !scriptable inline #end function compareExchange(expected:T, replacement:T):T {
		return this.compareExchange(expected, replacement);
	}

	public #if !scriptable inline #end function exchange(value:T):T {
		return this.exchange(value);
	}

	public #if !scriptable inline #end function load():T {
		return this.load();
	}

	public #if !scriptable inline #end function store(value:T):T {
		return this.store(value);
	}
	#end
}
