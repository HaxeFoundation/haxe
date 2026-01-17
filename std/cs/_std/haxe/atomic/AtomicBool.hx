package haxe.atomic;

private class BoolWrapper {
	public var value:Int; // Store as int for Interlocked operations

	public function new(value:Bool) {
		this.value = value ? 1 : 0;
	}
}

abstract AtomicBool(BoolWrapper) {
	public inline function new(value:Bool) {
		this = new BoolWrapper(value);
	}

	public inline function compareExchange(expected:Bool, replacement:Bool):Bool {
		var expectedInt = expected ? 1 : 0;
		var replacementInt = replacement ? 1 : 0;
		var original:Int = cs.Syntax.code("System.Threading.Interlocked.CompareExchange(ref ({0}).value, {1}, {2})", this, replacementInt,
			expectedInt);
		return original != 0;
	}

	public inline function exchange(value:Bool):Bool {
		var valueInt = value ? 1 : 0;
		var original:Int = cs.Syntax.code("System.Threading.Interlocked.Exchange(ref ({0}).value, {1})", this, valueInt);
		return original != 0;
	}

	public inline function load():Bool {
		return this.value != 0; // according to the CLI spec reads and writes are atomic
	}

	public inline function store(value:Bool):Bool {
		this.value = value ? 1 : 0; // according to the CLI spec reads and writes are atomic
		return value;
	}
}
