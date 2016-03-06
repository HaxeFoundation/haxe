package unit;

private interface IEmpty {
}

private interface Foo {
	public function foo() : String;
}

private interface IX {
	public function getX() : Float;
}

private interface IX2 {
	public function getX() : Float;
}

private interface IY extends Foo {
	public function getY() : Float;
}

private class Point implements IX implements IY {

	var x : Float;
	var y : Int;

	public function new(x,y) {
		this.x = x;
		this.y = y;
	}

	public function getX() {
		return x;
	}

	public function getY() : Int {
		return y;
	}

	public function foo() {
		return "bar";
	}

}

class TestInterface extends Test {

	public function test() {
		var p = new Point(1.3,5);
		var px : IX = p;
		var py : IY = p;
		t( Std.is(p, Point) );
		t( Std.is(p, IX) );
		t( Std.is(p, IY) );
		f( Std.is(p, IEmpty) );
		f( Std.is(p, IX2) );

		t( px == p );
		t( py == p );

		eq( px, p );
		eq( py, p );
		t( (px:Dynamic) == (py:Dynamic) );

		eq( px.getX(), 1.3 );
		eq( py.getY(), 5 );
		eq( py.foo(), "bar" );

		eq( (px:Dynamic).getX(), 1.3 );
		eq( (py:Dynamic).getY(), 5 );
		eq( (py:Dynamic).foo(), "bar" );

		var p2 = Std.instance(px,Point);
		eq( p, p2 );
		eq( cast(py,IX), px );
	}

}