package python;

import python.HaxeIterable;
import python.NativeIterator;

/**
    This type represents native python iterables (objects that implement __iter__() method).
    It supports haxe iteration and conversion to `Iterable` by creating wrapper objects.
**/
abstract NativeIterable<T>(NativeIterableRaw<T>) to NativeIterableRaw<T> from NativeIterableRaw<T> {

    /**
        Return haxe `Iterable` object wrapping `this` native iterable.
    **/
	@:to public inline function toHaxeIterable():HaxeIterable<T> return new HaxeIterable(this);

    /**
        Return haxe `Iterator` object by wrapping `this.__iter__()` native iterator.
    **/
	public inline function iterator():HaxeIterator<T> return new HaxeIterator(this.__iter__());
}

/**
    Native python iterable protocol.
**/
typedef NativeIterableRaw<T> = {
	function __iter__():NativeIterator<T>;
}
