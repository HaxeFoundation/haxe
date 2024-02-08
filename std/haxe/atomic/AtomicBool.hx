package haxe.atomic;

#if !(target.atomics || core_api)
#error "Atomic operations are not supported on this target!"
#end

/**
	Atomic boolean.
	(js) The Atomics and SharedArrayBuffer objects need to be available. Errors will be thrown if this is not the case.
**/
@:coreApi
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

	/**
		Atomically compares the value of `a` with `expected` and replaces `a` with `replacement` if they are equal..
		Returns the original value of `a`.
	**/
	public inline function compareExchange(expected:Bool, replacement:Bool):Bool {
		return toBool(this.compareExchange(toInt(expected), toInt(replacement)));
	}

	/**
		Atomically exchanges `a` with `value`.
		Returns the original value of `a`.
	**/
	public inline function exchange(value:Bool):Bool {
		return toBool(this.exchange(toInt(value)));
	}

	/**
		Atomically fetches the value of `a`.
	**/
	public inline function load():Bool {
		return toBool(this.load());
	}

	/**
		Atomically stores `value` into `a`.
		Returns the value that has been stored.
	**/
	public inline function store(value:Bool):Bool {
		return toBool(this.store(toInt(value)));
	}
}
