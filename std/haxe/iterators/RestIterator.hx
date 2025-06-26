package haxe.iterators;

class RestIterator<T> {
	final args:Rest<T>;
	var current:Int = 0;

	@:allow(haxe.Rest)
	inline function new(args:Any) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():T {
		return args[current++];
	}
}