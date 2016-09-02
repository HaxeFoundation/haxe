package unit.issues;

class Issue5432 extends unit.Test {
	function test() {
		var pt = new Point();

		function closure()
		{
			pt.x = pt.x + pt.y;
		}

		closure();
		feq(3., pt.x);
		feq(2., pt.y);
	}

}

private class Point
{
	public var x:Float = 1;
	public var y:Float = 2;

	@:extern inline public function new() {}
}