
package python;

import python.HaxeIterable;
import python.NativeIterator;

abstract NativeIterable <T>(NativeIterableRaw<T>) to NativeIterableRaw<T> from NativeIterableRaw<T> {
	@:to public inline function toHaxeIterable():HaxeIterable<T> return new HaxeIterable(this);

	public inline function iterator():HaxeIterator<T> return toHaxeIterable().iterator();

	public inline function getNativeIterable():NativeIterableRaw<T> return this;
	public inline function getNativeIterator():NativeIteratorRaw<T> return this.__iter__();

}
typedef NativeIterableRaw<T> = {
	function __iter__():NativeIterator<T>;
}
