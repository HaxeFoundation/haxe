import haxe.ds.WeakMap;
using Lambda;

/**
	The goal with this test is to test the 'weak' behaviour of WeakMap.
	If it works, it will run forever, with constant memory usage;
	Otherwise it will exhaust the memory
**/
class TestWeakMap
{

	static function main()
	{
#if sys
		var iterate = Sys.args().has('-iterate');
#else
		var iterate = true;
#end
		var map = new WeakMap(),
				saved = new List();
		var i = 0;
		while(true)
		{
			i++;
			var obj = { count:i };
			map.set(obj, i);
			if (i % 77 == 0)
			{
				// test also some strong references
				if (i & 1 == 0)
					saved.add(obj);
				else
					saved.push(obj);
				if (saved.length > 1000)
					saved.pop();

				//check if all references are still present
				for (s in saved)
				{
					var val = map.get(s);
					if (val == null)
						trace('ERROR: Reference from $s not found!');
					if (val != s.count)
						trace('ERROR: Wrong value for $s: $val');
				}
			} else if (iterate && i % 10000 == 0) {
				var count = 0;
				for (v in map.keys())
				{
					count++;
					if (v == null) trace("ITERATION ERROR:NULL");
					var val = map.get(v);
					if (val != v.count)
						trace('ITERATION ERROR: $val != ${v.count}');
				}
#if false
				Sys.print('$count (${count / saved.length}) \r');
#else
				trace(count, count / saved.length);
#end
			}
		}
	}

}
