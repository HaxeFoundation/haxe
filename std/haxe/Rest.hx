package haxe;

/**
	A special type that represents "rest" function argument.

	Should be used as a type for the last argument of a method, representing that
	arbitrary number of arguments of given type can be passed to that method.

	Allows to use array access by index to get values of rest arguments.
	If the index exceeds the amount of rest arguments passed, the result is unspecified.
**/
@:coreType abstract Rest<T> {
	public var length(get,never):Int;
	extern function get_length():Int;

	@:arrayAccess extern function get(index:Int):T;

	@:to public inline function toArray():Array<T> {
		return [for(i in 0...length) get(i)];
	}

	public inline function iterator():RestIterator<T> {
		return new RestIterator<T>(this);
	}

	public inline function keyValueIterator():RestKeyValueIterator<T> {
		return new RestKeyValueIterator<T>(this);
	}
}

private class RestIterator<T> {
	final args:Rest<T>;
	var current:Int = 0;

	public inline function new(args:Any) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():T {
		return args[current++];
	}
}

private class RestKeyValueIterator<T> {
	final args:Rest<T>;
	var current:Int = 0;

	public inline function new(args:Any) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:args[current++]};
	}
}