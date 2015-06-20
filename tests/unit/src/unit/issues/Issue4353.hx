package unit.issues;

private class Iter1 {
	public var i : Int;
	public var e : Int;
	public inline function new(i, e) {this.i = i; this.e = e;}
	public inline function hasNext() return i < e;
	public inline function next() {var j = i; i++; return j;}
}

private class Iter2 {
	public var i : Int;
	public var e : Int;
	var j : Int;
	public inline function new(i, e) {this.i = i; this.e = e;}
	public inline function hasNext() return i < e;
	public inline function next() {j = i; i++; return j;}
}


class Issue4353 extends Test {
	function test() {
		var s = 0;
		for (i in new Iter1(0, 10)) {
			s += i;
		}
		eq(45, s);
	}

	function test2() {
		var s = 0;
		for (i in new Iter2(0, 10)) {
			s += i;
		}
		eq(45, s);
	}
}