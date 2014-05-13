
package python;

import python.HaxeIterable;
import python.NativeIterator;

abstract NativeIterable <T>(NativeIterableRaw<T>) to NativeIterableRaw<T> from NativeIterableRaw<T> {
	@:to public static inline function toHaxeIterable <T>(p:NativeIterableRaw<T>):HaxeIterable<T> return new HaxeIterable(p);

	//@:from public static inline function fromArray <T>(p:Array<T>):PyIterable<T> return cast p;

	public inline function iterator <T>():HaxeIterator<T> return toHaxeIterable().iterator();

	public function getNativeIterable <T>():NativeIterableRaw<T> return this;
	public function getNativeIterator <T>():NativeIteratorRaw<T> return this.__iter__();

}
typedef NativeIterableRaw<T> = {
	function __iter__():NativeIterator<T>;
}
