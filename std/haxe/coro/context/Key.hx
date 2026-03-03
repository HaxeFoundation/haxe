package haxe.coro.context;

class KeyImpl<T> {
	static var counter = 0;
	static var counterMutex = new sys.thread.Mutex(); // TODO: AtomicInt once it's cross-platform

	public final name:String;
	public final id:Int;

	function new(id:Int, name:String) {
		this.name = name;
		this.id = id;
	}

	static public function createNew<T>(name:String) {
		counterMutex.acquire();
		var id = counter++;
		counterMutex.release();
		return new KeyImpl<T>(id, name);
	}
}

private typedef WithKey<K> = {
	final key:Key<K>;
}

/**
	Represents the key of a `Context` element. See `haxe.coro.dispatchers.Dispatcher` for a usage example.
**/
@:forward
@:forward.statics
extern abstract Key<T>(KeyImpl<T>) {
	/**
		Creates a new key with a given `name`. The name is purely descriptive.
	**/
	public inline function new(name:String) {
		this = KeyImpl.createNew(name);
	}

	@:noCompletion
	@:from static public inline function fromClass<K, C:(Class<Any> & WithKey<K>)>(c:C):Key<K> {
		return c.key;
	}
}