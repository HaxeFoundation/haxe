package haxe.atomic;

#if cppia
extern
#end
abstract AtomicInt(cpp.Pointer<Int>) {
	#if cppia
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
	#else
	public #if !scriptable inline #end function new(value:Int) {
		this = cpp.Pointer.ofArray([value]);
	}

	public #if !scriptable inline #end function add(b:Int):Int {
		return untyped __cpp__("_hx_atomic_add({0}, {1})", this, b);
	}

	public #if !scriptable inline #end function sub(b:Int):Int {
		return untyped __cpp__("_hx_atomic_sub({0}, {1})", this, b);
	}

	public #if !scriptable inline #end function and(b:Int):Int {
		return untyped __cpp__("_hx_atomic_and({0}, {1})", this, b);
	}

	public #if !scriptable inline #end function or(b:Int):Int {
		return untyped __cpp__("_hx_atomic_or({0}, {1})", this, b);
	}

	public #if !scriptable inline #end function xor(b:Int):Int {
		return untyped __cpp__("_hx_atomic_xor({0}, {1})", this, b);
	}

	public #if !scriptable inline #end function compareExchange(expected:Int, replacement:Int):Int {
		return untyped __cpp__("_hx_atomic_compare_exchange({0}, {1}, {2})", this, expected, replacement);
	}

	public #if !scriptable inline #end function exchange(value:Int):Int {
		return untyped __cpp__("_hx_atomic_exchange({0}, {1})", this, value);
	}

	public #if !scriptable inline #end function load():Int {
		return untyped __cpp__("_hx_atomic_load({0})", this);
	}

	public #if !scriptable inline #end function store(value:Int):Int {
		return untyped __cpp__("_hx_atomic_store({0}, {1})", this, value);
	}
	#end
}
