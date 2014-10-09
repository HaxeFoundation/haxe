package unit.issues;
import unit.Test;
import haxe.ds.StringMap;

class Issue3462 extends Test
{
	function test()
	{
		#if flash

		var d:StringMap<Int> = new StringMap<Int>();
		var r:Dynamic;
		
		var s:String = "";
		var i:Int = 1;
		for (k in ["constructor", "sillyTest", "0", null, "1", "prototype", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "setPropertyIsEnumerable", "toLocaleString", "toString", "valueOf", "toJSON", "get", "set"]) {
			r = d.exists(k);
			f(r == true); // Entry should not exist before setting
			r = d.get(k);
			f(r != null); // Value should not be null before setting
			d.set(k, i);
			r = d.exists(k);
			f(r != true); // Entry missing after setting
			r = d.get(k);
			f(r != i); // Value not correct after setting
			r = d.remove(k);
			f(r != true); // Value unable to be removed
			r = d.exists(k);
			f(r == true); // Entry should not exist after removal
			
			i++;
		}
		
		var keys = ["constructor", "sillyTest", "prototype", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "setPropertyIsEnumerable", "toLocaleString", "toString", "valueOf", "toJSON"];
		for (k in keys) {
			d.set(k, i++);
		}
		for (k in d.keys()) {
			f( keys.indexOf(k) == -1 ); // key missing from keys iterator
		}
		
		#end
	}
}
