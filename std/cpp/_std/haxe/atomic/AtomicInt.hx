package haxe.atomic;

#if cppia
extern
#end
abstract AtomicInt(cpp.Pointer<Int>) {
	public #if !(scriptable || cppia) inline #end function new(value:Int) {
		this = cpp.Pointer.ofArray([value]);
	}

	public #if !(scriptable || cppia) inline #end function add(b:Int):Int {
		return untyped __cpp__("_hx_atomic_add({0}, {1})", this, b);
	}

	public #if !(scriptable || cppia) inline #end function sub(b:Int):Int {
		return untyped __cpp__("_hx_atomic_sub({0}, {1})", this, b);
	}

	public #if !(scriptable || cppia) inline #end function and(b:Int):Int {
		return untyped __cpp__("_hx_atomic_and({0}, {1})", this, b);
	}

	public #if !(scriptable || cppia) inline #end function or(b:Int):Int {
		return untyped __cpp__("_hx_atomic_or({0}, {1})", this, b);
	}

	public #if !(scriptable || cppia) inline #end function xor(b:Int):Int {
		return untyped __cpp__("_hx_atomic_xor({0}, {1})", this, b);
	}

	public #if !(scriptable || cppia) inline #end function compareExchange(expected:Int, replacement:Int):Int {
		return untyped __cpp__("_hx_atomic_compare_exchange({0}, {1}, {2})", this, expected, replacement);
	}

	public #if !(scriptable || cppia) inline #end function exchange(value:Int):Int {
		return untyped __cpp__("_hx_atomic_exchange({0}, {1})", this, value);
	}

	public #if !(scriptable || cppia) inline #end function load():Int {
		return untyped __cpp__("_hx_atomic_load({0})", this);
	}

	public #if !(scriptable || cppia) inline #end function store(value:Int):Int {
		return untyped __cpp__("_hx_atomic_store({0}, {1})", this, value);
	}
}
