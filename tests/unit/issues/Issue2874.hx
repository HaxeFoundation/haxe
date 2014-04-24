package unit.issues;

class Issue2874 extends unit.Test
{
	private function test()
	{
		var a = new Arr(["1","2"]);
		var a2 = a.map(function(v) return Std.parseInt(v));
		eq(1,a2[0]);
		eq(2,a2[1]);
		// var a3:Arr<Int> = a.map(function(v) return Std.parseInt(v));
		// eq(1,a3[0]);
		// eq(2,a3[1]);
	}

	private function testArray()
	{
		var a = ["1","2"];
		var a2 = a.map(function(v) return Std.parseInt(v));
		eq(1,a2[0]);
		eq(2,a2[1]);
		// var a3:Array<Int> = a.map(function(v) return Std.parseInt(v));
		// eq(1,a3[0]);
		// eq(2,a3[1]);
	}
}

@:arrayAccess abstract Arr<T>(Array<T>)
{
	@:extern inline public function new(a)
	{
		this = a;
	}

	@:extern inline public function map<X>(fn:T->X):Arr<X>
	{
		var arr2 = [];
		for (v in this)
		{
			arr2.push(fn(v));
		}
		return new Arr(arr2);
	}
}
