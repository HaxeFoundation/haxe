package unit.issues;

class Issue11851 extends Test {
	function test() {
		var o = {arr: [{foo: 1, bar: 42}]};
		var arr = Reflect.field(o, "arr");

		var buf = new StringBuf();
		function append(s:Dynamic) {
			buf.add(s);
			buf.add(" ");
		}

		for (i in 0...arr.length) {
			append(arr[i].foo);
			append(arr[i].bar); // fails too
			append(arr[0].bar == null ? 42 : arr[0].bar);
		}
		eq("1 42 42 ", buf.toString());
	}
}
