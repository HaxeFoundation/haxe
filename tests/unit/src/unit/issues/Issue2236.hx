package unit.issues;

private class ArrayIterator<T> {
    var a : Array<T>;
    var pos : Int;
    public inline function new(a) {
        this.a = a;
        this.pos = 0;
    }
    public inline function hasNext() {
        return pos < a.length;
    }
    public inline function next() {
        return a[pos++];
    }
}

private abstract ArrayRead<T>(Array<T>) {

    public inline function new(a) {
        this = a;
    }

    function toArray() : Array<T> {
        return this;
    }

    public inline function iterator() : ArrayIterator<T> {
        return new ArrayIterator(this);
    }

}

class Issue2236 extends Test {
	function test() {
		var a = new ArrayRead([0]);
		for( x in a ) {

		}
	}
}