package unit.issues;

class Issue6486 extends Test
{
  function test() {
    var p = new Point();
    eq(p.getY(), 10);
    var iy:IY = p;
    eq(p.getY(), 10);
  }
}

@:keep private interface IY {
	function getY():Float;
}

@:keep private class Point implements IY {
	public function new() {}

	public function getY():Int {
		return 10;
	}
}