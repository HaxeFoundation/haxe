package haxe;

import java.NativeArray;

@:coreType
abstract Rest<T> {
	public var length(get,never):Int;
	inline function get_length():Int {
		return untyped __java__('{0}.length', this);
	}

	@:arrayAccess inline function get(i:Int):T {
		return untyped __java__('{0}[{1}]', this, i);
	}

	@:to public inline function toArray():Array<T> {
		return [for(i in 0...length) untyped __java__('{0}[{1}]', this, i)];
	}

	public inline function iterator():RestIterator<T> {
		return new RestIterator<T>(cast this);
	}

	public inline function keyValueIterator():RestKeyValueIterator<T> {
		return new RestKeyValueIterator<T>(cast this);
	}
}

private class RestIterator<T> {
	final args:NativeArray<T>;
	var current:Int = 0;

	public inline function new(args:NativeArray<T>) {
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
	final args:NativeArray<T>;
	var current:Int = 0;

	public inline function new(args:NativeArray<T>) {
		this.args = args;
	}

	public inline function hasNext():Bool {
		return current < args.length;
	}

	public inline function next():{key:Int, value:T} {
		return {key:current, value:args[current++]};
	}
}