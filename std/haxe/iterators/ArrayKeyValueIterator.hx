package haxe.iterators;

@:ifFeature("anon_read.keyValueIterator", "dynamic_read.keyValueIterator")
class ArrayKeyValueIterator<T> {
	var idx:Int;
	var arr:Array<T>;

	public inline function new(arr:Array<T>) {
		this.arr = arr;
		idx = 0;
	}

	public inline function hasNext():Bool {
		return idx < arr.length;
	}

	public inline function next():{key:Int,value:T} {
		return {value:arr[idx], key:idx++};
	}
}
