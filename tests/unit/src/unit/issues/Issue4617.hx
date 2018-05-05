package unit.issues;

import haxe.ds.ObjectMap;

private class CTest<T:{}>
{
    var map:ObjectMap<T, Array<Bool>>;

    public function new()
    {
        map = new ObjectMap();
    }

    public function set(k:T)
    {
        map.set(k, [true]);
    }

    public function check(keys:Array<T>)
    {
        var value;

		var s = "";
        for (k in keys)
        {
            value = map.get(k);
			s += (value == null);
        }
		return s;
    }
}

class Issue4617 extends Test {
	function test() {
        var t:CTest<{test:Int}> = new CTest();

        var key1 =  { test:1 };
        var key2 =  { test:2 };
        t.set(key1);
        eq("falsetrue", t.check([key1, key2]));
	}
}