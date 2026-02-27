package haxe.coro;

#if target.threaded
/**
	Thread-local storage. On `target.threaded` targets, each thread has its own value.
	On single-threaded targets this is a plain field.
**/
class Tls<T> {
	final inner:sys.thread.Tls<T>;

	public var value(get, set):Null<T>;

	public function new() {
		inner = new sys.thread.Tls();
	}

	inline function get_value():Null<T> {
		try {
			return inner.value;
		} catch (_:Dynamic) {
			return null;
		}
	}

	inline function set_value(v:Null<T>):Null<T> {
		inner.value = v;
		return v;
	}
}
#else
/**
	Thread-local storage. On `target.threaded` targets, each thread has its own value.
	On single-threaded targets this is a plain field.
**/
class Tls<T> {
	public var value:Null<T>;

	public function new() {}
}
#end
