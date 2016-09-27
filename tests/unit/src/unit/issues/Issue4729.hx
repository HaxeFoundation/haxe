package unit.issues;

class Issue4729 extends Test {
	function test() {
		var raw = "Hello world !!!";

		var cursor:Int = 0;
		inline function adv(char:String = " ", ?start:Int) {
			cursor = raw.indexOf(char, start);
			return cursor = (cursor > -1 ? cursor : raw.length) + char.length;
		}

		var hello = raw.substring(0, adv());
		var world = raw.substring(cursor, adv(cursor));
		eq("Hello ", hello);
		eq("world ", world);
	}
}