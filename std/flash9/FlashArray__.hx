package flash;

private class ArrayIterator<T> {

	var arr : Array<T>;
	var cur : Int;

	public function new(a) {
		arr = a;
		cur = 0;
	}

	public function hasNext() {
		return cur < arr.length;
	}

	public function next() {
		return arr[cur++];
	}

}

class FlashArray__<T> extends Array<T> {

	public override function insert(i,x) {
		untyped (__native__.splice)(i,0,x);
	}

	public override function copy() {
		return untyped (__native__.slice)();
	}

	public override function remove(obj) untyped {
		for( i in 0...length ) {
			if( this[i] == obj ) {
				(__native__.splice)(i,1);
				return true;
			}
		}
		return false;
	}

	public override function iterator() : Iterator<T> untyped {
		return new ArrayIterator(this);
	}

}

