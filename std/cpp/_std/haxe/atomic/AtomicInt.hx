package haxe.atomic;

@:include("hx/atomic/AtomicInt.hpp")
@:cpp.ManagedType({type: "AtomicInt", namespace: ["hx", "atomic"], flags: [StandardNaming]})
private extern class AtomicIntImpl {
	function new(value:Int);

	function fetchAdd(value:Int):Int;
	function fetchSub(value:Int):Int;
	function fetchAnd(value:Int):Int;
	function fetchOr(value:Int):Int;
	function fetchXor(value:Int):Int;
	function load():Int;
	function store(value:Int):Int;
	function exchange(value:Int):Int;
	function compareExchange(expected:Int, replacement:Int):Int;
}

private typedef AtomicIntData = AtomicIntImpl;

#if cppia
extern
#end
abstract AtomicInt(AtomicIntData) {
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
	public #if !scriptable inline #end function new(value:Int):Void {
		this = new AtomicIntImpl(value);
	}

	public #if !scriptable inline #end function add(b:Int):Int {
		return this.fetchAdd(b);
	}

	public #if !scriptable inline #end function sub(b:Int):Int {
		return this.fetchSub(b);
	}

	public #if !scriptable inline #end function and(b:Int):Int {
		return this.fetchAnd(b);
	}

	public #if !scriptable inline #end function or(b:Int):Int {
		return this.fetchOr(b);
	}

	public #if !scriptable inline #end function xor(b:Int):Int {
		return this.fetchXor(b);
	}

	public #if !scriptable inline #end function compareExchange(expected:Int, replacement:Int):Int {
		return this.compareExchange(expected, replacement);
	}

	public #if !scriptable inline #end function exchange(value:Int):Int {
		return this.exchange(value);
	}

	public #if !scriptable inline #end function load():Int {
		return this.load();
	}

	public #if !scriptable inline #end function store(value:Int):Int {
		return this.store(value);
	}
	#end
}
