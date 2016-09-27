package unit.issues;
import haxe.ds.Vector;

class Issue3303 extends Test {
	function test() {
		var ret = createClosure(null);
		eq(null,ret());
	}

	static function createClosure<T>(v:Vector<T>):Void->Vector<T>
	{
		return function() return v;
	}
}
