package hxcoro.concurrent;

#if (hl || jvm)
typedef AtomicObject<T:{}> = haxe.atomic.AtomicObject<T>;
#else
import haxe.coro.Mutex;

typedef AtomicObject<T:{}> = AtomicObjectImpl<T>;

private class AtomicObjectImpl<T:{}> {
	final mutex : Mutex;
	var object : T;

	public function new(object) {
		mutex = new Mutex();
		this.object = object;
	}
	
	public function compareExchange(expected : T, replacement : T) {
		mutex.acquire();

		return if (object == expected) {
			object = replacement;
			mutex.release();

			expected;
		} else {
			final current = object;

			mutex.release();

			current;
		}
	}

	public function exchange(replacement : T) {
		mutex.acquire();

		final current = object;

		object = replacement;

		mutex.release();

		return current;
	}

	public function load() {
		mutex.acquire();

		final current = object;

		mutex.release();

		return current;
	}

	public function store(replacement : T) {
		mutex.acquire();

		object = replacement;

		mutex.release();

		return replacement;
	}
}
#end