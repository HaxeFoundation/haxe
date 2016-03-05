package unit.issues;
import unit.Test;
import haxe.ds.IntMap;

class Issue3762 extends Test
{
	function test()
	{
		// test fast iterators against normal keys
		var d = new IntMap<Int>();
		var keys = [1, 2, 3];
		for (i in 0...keys.length) {
			var k = keys[i];
			d.set(k, i);
		}
		var keyCount:Int = 0;
		for (k in d.keys()) {
			f( keys.indexOf(k) == -1 ); // key missing from keys iterator
			keyCount++;
		}
		f( keyCount != keys.length ); // keys missing
		var keyCount:Int = 0;
		for (v in d) {
			f(v < 0 || v >= keys.length); // bad value iterated
			keyCount++;
		}
		f(keyCount != keys.length); // values missing

	}
}
