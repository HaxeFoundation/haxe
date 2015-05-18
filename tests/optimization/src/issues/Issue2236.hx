package issues;

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

class Issue2236 {
	@:js('
		var a = [0];
		var _g_a = a;
		var _g_pos = 0;
		while(_g_pos < _g_a.length) {
			var x = _g_a[_g_pos++];
		}
	')
	static function test() {
		var a = new ArrayRead([0]);
		for( x in a ) {
			x;
		}
	}
}