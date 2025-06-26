package haxe.iterators;

class RestKeyValueIterator<T> {
	final args:Rest<T>;
	var current:Int = 0;

	@:allow(haxe.Rest)
	inline function new(args:Any) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:args[current++]};
	}
}