
package python;

import python.NativeIterable.NativeIterableRaw;

abstract NativeIterator <T>(NativeIteratorRaw<T>) to NativeIteratorRaw<T> to NativeIterable<T> {
	public inline function new (p:NativeIteratorRaw<T>) this = p;

	@:to public static inline function toHaxeIterator <T>(p:NativeIteratorRaw<T>):HaxeIterator<T> return new HaxeIterator(p);
	@:to public static inline function toNativeIterable <T>(p:NativeIteratorRaw<T>):NativeIterable<T> return p;

	public function getNativeIteratorRaw <T>():NativeIteratorRaw<T> return this;
}

typedef NativeIteratorRaw<T> = {
	> NativeIterableRaw<T>,
	function __next__ ():T;
	//function __iter__ ():NativeIterator<T>;
}