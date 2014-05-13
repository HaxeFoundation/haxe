
package python;

import python.NativeIterable.NativeIterableRaw;

abstract NativeIterator <T>(NativeIteratorRaw<T>) to NativeIteratorRaw<T> to NativeIterable<T> {
	public inline function new (p:NativeIteratorRaw<T>) this = p;

	@:to public inline function toHaxeIterator():HaxeIterator<T> return new HaxeIterator(this);
	@:to public inline function toNativeIterable():NativeIterable<T> return this;

	public inline function getNativeIteratorRaw():NativeIteratorRaw<T> return this;
}

typedef NativeIteratorRaw<T> = {
	> NativeIterableRaw<T>,
	function __next__ ():T;
}