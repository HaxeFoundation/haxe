package haxe;

import python.Syntax;

@:coreType @:runtimeValue @:semantics(value)
abstract Rest<T> {
	public var length(get,never):Int;
	inline function get_length():Int {
		return Syntax.code('len({0})', this);
	}

	@:arrayAccess inline function get(i:Int):T {
		return Syntax.code('{0}[{1}]', this, i);
	}

	@:to public inline function toArray():Array<T> {
		return [for(i in 0...length) Syntax.code('{0}[{1}]', this, i)];
	}

	public inline function iterator():RestIterator<T> {
		return new RestIterator<T>(cast this);
	}

	public inline function keyValueIterator():RestKeyValueIterator<T> {
		return new RestKeyValueIterator<T>(cast this);
	}
}

private class RestIterator<T> {
	final args:Array<T>;
	var current:Int = 0;

	public inline function new(args:Array<T>) {
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
	final args:Array<T>;
	var current:Int = 0;

	public inline function new(args:Array<T>) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:args[current++]};
	}
}