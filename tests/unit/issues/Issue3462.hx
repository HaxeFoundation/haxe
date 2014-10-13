package unit.issues;
import unit.Test;
import haxe.ds.StringMap;

class Issue3462 extends Test
{
	function test()
	{
		var d:StringMap<Int> = new StringMap<Int>();
		var r:Dynamic;
		
		var s:String = "";
		var keys = ["__proto__", "__definegetter__", "__definesetter__", "__lookupgetter__", "__lookupsetter__", "constructor", "sillyTest", "0", "1", "prototype", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "setPropertyIsEnumerable", "toLocaleString", "toString", "valueOf", "toJSON", "get", "set"];
		for (i in 0...keys.length) {
			var k = keys[i];
			r = d.exists(k);
			f(r == true); // Entry should not exist before setting
			r = d.get(k);
			f(r != null); // Value should not be null before setting
			d.set(k, i);
			r = d.exists(k);
			f(r != true); // Entry missing after setting
		}
		
		var keyCount:Int = 0;
		for (k in d.keys()) {
			f( keys.indexOf(k) == -1 ); // key missing from keys iterator
			keyCount++;
		}
		f(keyCount != keys.length); // keys missing
		var keyCount:Int = 0;
		for (v in d) {
			f(v < 0 || v >= keys.length); // bad value iterated
			keyCount++;
		}
		f(keyCount != keys.length); // values missing
		for (i in 0...keys.length) {
			var k = keys[i];
			r = d.get(k);
			f(r != i); // Value not correct after setting
			r = d.remove(k);
			f(r != true); // Value unable to be removed
			r = d.exists(k);
			f(r == true); // Entry should not exist after removal
		}
		keyCount = 0;
		for (k in d.keys()) {			
		}
		f( keyCount != 0 ); // keys remaining after removal
		
		// test fast iterators against normal keys
		d = new StringMap();
		keys = ["1", "2", "3"];
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
