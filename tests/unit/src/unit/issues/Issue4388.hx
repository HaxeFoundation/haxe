package unit.issues;
import haxe.Int64;

class Issue4388 extends Test
{
	public function test()
	{
		var test = new PTest();
		test.x |= 2;
		t(test.x == 3);
	}
}

private class PTest
{
	public var x(default, set) : Int64;
	inline function set_x(i:Int64) : Int64 {
		return this.x = i;
	}

	public function new() { this.x = 1; }
}
