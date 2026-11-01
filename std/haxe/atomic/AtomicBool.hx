package haxe.atomic;

private typedef AtomicBoolData = AtomicInt;

/**
	Atomic boolean.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
#if eval
@:native("haxe.atomic.AtomicBool")
#end
abstract AtomicBool(AtomicBoolData) {
	public function new(value:Bool):Void {
		this = new AtomicInt(toInt(value));
	}

	private inline function toInt(v:Bool):Int {
		return v ? 1 : 0;
	}

	private inline function toBool(v:Int):Bool {
		return v == 1;
	}

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public function compareExchange(expected:Bool, replacement:Bool):Bool {
		return toBool(this.compareExchange(toInt(expected), toInt(replacement)));
	}

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public function exchange(value:Bool):Bool {
		return toBool(this.exchange(toInt(value)));
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public function load():Bool {
		return toBool(this.load());
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public function store(value:Bool):Bool {
		return toBool(this.store(toInt(value)));
	}
}