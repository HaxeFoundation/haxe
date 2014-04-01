package python;

import python.lib.Types;

class HaxeIterable<T> {

	var x :NativeIterable<T>;

	public inline function new (x:NativeIterable<T>) {
		this.x = x;
	}

	public inline function iterator ():HaxeIterator<T> return new HaxeIterator(x.__iter__());
}