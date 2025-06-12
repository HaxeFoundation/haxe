package haxe.atomic;

#if doc_gen
@:coreApi
@:coreType
abstract AtomicBool {
	public function new(value:Bool):Void;

	public function compareExchange(expected:Bool, replacement:Bool):Bool;

	public function exchange(value:Bool):Bool;

	public function load():Bool;

	public function store(value:Bool):Bool;
}
#else
abstract AtomicBool(AtomicInt) {
	private inline function toInt(v:Bool):Int {
		return v ? 1 : 0;
	}

	private inline function toBool(v:Int):Bool {
		return v == 1;
	}

	public inline function new(value:Bool):Void {
		this = new AtomicInt(toInt(value));
	}

	public inline function compareExchange(expected:Bool, replacement:Bool):Bool {
		return toBool(this.compareExchange(toInt(expected), toInt(replacement)));
	}

	public inline function exchange(value:Bool):Bool {
		return toBool(this.exchange(toInt(value)));
	}

	public inline function load():Bool {
		return toBool(this.load());
	}

	public inline function store(value:Bool):Bool {
		return toBool(this.store(toInt(value)));
	}
}
#end
