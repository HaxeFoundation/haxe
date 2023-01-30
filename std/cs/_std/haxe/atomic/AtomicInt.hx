package haxe.atomic;

private class IntWrapper {
	public var value:Int;

	public function new(value:Int) {
		this.value = value;
	}
}

abstract AtomicInt(IntWrapper) {
	public inline function new(value:Int) {
		this = new IntWrapper(value);
	}

	private inline function cas_loop(value:Int, op:(a:Int, b:Int) -> Int):Int {
		var oldValue;
		var newValue;
		do {
			oldValue = load();
			newValue = op(oldValue, value);
		} while(compareExchange(oldValue, newValue) != oldValue);
		return oldValue;
	}

	public inline function add(b:Int):Int {
		return cas_loop(b, (a, b) -> a + b);
	}

	public inline function sub(b:Int):Int {
		return cas_loop(b, (a, b) -> a - b);
	}

	public inline function and(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a & b);
	}

	public inline function or(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a | b);
	}

	public inline function xor(b:Int):Int {
		return cas_loop(b, (a, b) -> cast a ^ b);
	}

	public inline function compareExchange(expected:Int, replacement:Int):Int {
		return cs.Syntax.code("System.Threading.Interlocked.CompareExchange(ref ({0}).value, {1}, {2})", this, replacement, expected);
	}

	public inline function exchange(value:Int):Int {
		return cs.Syntax.code("System.Threading.Interlocked.Exchange(ref ({0}).value, {1})", this, value);
	}

	public inline function load():Int {
		return this.value; // according to the CLI spec reads and writes are atomic
	}

	public inline function store(value:Int):Int {
		return this.value = value; // according to the CLI spec reads and writes are atomic
	}
}