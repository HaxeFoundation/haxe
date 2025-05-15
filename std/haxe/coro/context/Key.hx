package haxe.coro.context;

class Key<T> {
	static var counter = 0;
	static var counterMutex = new Mutex();

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
		return new Key<T>(id, name);
	}
}