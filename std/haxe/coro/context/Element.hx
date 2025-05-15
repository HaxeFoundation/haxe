package haxe.coro.context;

abstract class Element<T> {
	public final id:Key<T>;

	function new(id:Key<T>) {
		this.id = id;
	}

	abstract public function toString():String;
}