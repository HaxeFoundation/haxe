package unit.issues;

class Issue3023 extends Test {
    function test() {
			var x:Array<Null<Map<Int,CTest>>> = [];
			x.push([ 0 => new CTest() ]);
			eq(x[0].get(0).x, 10);
			var y = x[0].get(0).x;
			eq(y,10);
		}
}

private class CTest
{
	public var x:Int = 10;
	public function new()
	{
	}
}
