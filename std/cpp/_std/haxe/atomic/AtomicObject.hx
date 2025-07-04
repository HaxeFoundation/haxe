package haxe.atomic;

private class AtomicObjectImpl<T> {
	public final lock : sys.thread.Mutex;

	public var obj : T;

	public function new(obj) {
		this.lock = new sys.thread.Mutex();
		this.obj  = obj;
	}
}

abstract AtomicObject<T:{}>(AtomicObjectImpl<T>) {
	public function new(value:T) {
		this = new AtomicObjectImpl(value);
	}

	public function compareExchange(expected:T, replacement:T):T {
		this.lock.acquire();
		return if (this.obj == expected) {
			final current = this.obj;
			this.obj = replacement;
			this.lock.release();

			current;
		} else {
			final current = this.obj;
			this.lock.release();
			
			current;
		}
	}

	public function exchange(value:T):T {
		this.lock.acquire();
		final current = this.obj;
		this.obj = value;
		this.lock.release();

		return current;
	}

	public function load():T {
		this.lock.acquire();
		final current = this.obj;
		this.lock.release();

		return current;
	}

	public function store(value:T):T {
		this.lock.acquire();
		this.obj = value;
		this.lock.release();

		return value;
	}
}