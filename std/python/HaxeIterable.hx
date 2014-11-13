package python;

import python.NativeIterable.NativeIterableRaw;

class HaxeIterable<T> {

	var x : NativeIterableRaw<T>;

	public inline function new(x:NativeIterableRaw<T>) {
		this.x = x;
	}

	public inline function iterator():HaxeIterator<T> {
        return new HaxeIterator(x.__iter__());
    }
}
