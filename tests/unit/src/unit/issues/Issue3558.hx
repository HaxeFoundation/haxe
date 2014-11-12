package unit.issues;
#if cs
import cs.system.collections.generic.*;
import cs.system.collections.*;
#end

class Issue3558 extends Test
{
#if cs
	public function test()
	{
		var dict = new Dictionary_2<Int,String>();
		dict[10] = "11";
		var e:IDictionaryEnumerator = dict.GetEnumerator();
		while (e.MoveNext())
		{
			eq(e.Key,10);
			eq(e.Value,"11");
		}
		var e = dict.GetEnumerator();
		while (e.MoveNext())
		{
			var cur = e.Current;
			eq(cur.Key,10);
			eq(cur.Value,"11");
		}
	}
#end

}
