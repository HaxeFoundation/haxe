package python;

import python.NativeIterable.NativeIterableRaw;

/**
    This type represents native python iterators.
    It supports automatic conversion to haxe `Iterator` by creating wrapper object.
**/
abstract NativeIterator<T>(NativeIteratorRaw<T>) to NativeIteratorRaw<T> to NativeIterable<T> {
	public inline function new(p:NativeIteratorRaw<T>) this = p;

    /**
        Return haxe `Iterator` object by wrapping `this` native iterator.
    **/
	@:to public inline function toHaxeIterator():HaxeIterator<T> return new HaxeIterator(this);
}

/**
    Native python iterator protocol.
**/
typedef NativeIteratorRaw<T> = {
	>NativeIterableRaw<T>,
	function __next__():T;
}
