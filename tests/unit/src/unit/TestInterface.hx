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

	public function getY() : #if java /* see https://github.com/HaxeFoundation/haxe/issues/6486 */ Float #else Int #end {
		return y;
	}

	public function foo() {
		return "bar";
	}

}

private interface I1 {
	function foo() : Void;
}

private interface I2 extends I1 {
	function bar() : Void;
}

private class C implements I2 {
	public function new() {};
	public function foo() {};
	public function bar() {};
}

class TestInterface extends Test {

	public function test() {
		var p = new Point(1.3,5);
		var px : IX = p;
		var py : IY = p;
		t( Std.isOfType(p, Point) );
		t( Std.isOfType(p, IX) );
		t( Std.isOfType(p, IY) );
		f( Std.isOfType(p, IEmpty) );
		f( Std.isOfType(p, IX2) );

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

		var p2 = Std.downcast(px,Point);
		eq( p, p2 );
		eq( cast(py,IX), px );

		var c = new C();
		var i2 : I2 = c;
		var i1 : I1 = c;
		eq(i1, c);
		eq(i2, c);
		eq(i1,i2);
	}
}