package unit.issues;

@:callable
private abstract A<T>(T) from T {}

private class DynCallBase {
	public function new() {}

	public function int(x:Int):String {
		return "A" + x;
	}

	public function float(x:Float):String {
		return "F" + x;
	}

	public function two(x:Int, y:Int):String {
		return x + "," + y;
	}
}

private class DynCallSub extends DynCallBase {
	override public function int(x:Int):String {
		return "B" + x;
	}
}

class Issue3218 extends Test {
	function test() {
		var a:A<Int->Int> = function(x) return x * 2;
		eq(2, a(1));

		var f:haxe.Constraints.Function = function(){ return 12; }
		eq(12, f());
	}

	static function dynCallStatic(x:Int):String {
		return "S" + x;
	}

	function testDynamicClosureCall() {
		var o:DynCallBase = new DynCallSub();
		var f = o.int;
		var d:Dynamic = f;
		eq("B4", d(4));
		var f = o.float;
		var d:Dynamic = f;
		eq("F1.5", d(1.5));
		var f = o.two;
		var d:Dynamic = f;
		eq("3,4", d(3, 4));
		var f = dynCallStatic;
		var d:Dynamic = f;
		eq("S5", d(5));
		var f = function(x:Int) return "L" + x;
		var d:Dynamic = f;
		eq("L6", d(6));
	}
}
