package unit.issues;

class Issue13241 extends Test {
	#if js
	function test() {
		var a:Array<Null<Int>> = [0];
		eq("null", a[1] + "");
		eq("0", a[0] + "");
		var s:Array<Null<String>> = [""];
		eq("[null]", "[" + s[1] + "]");
		eq("[]", "[" + s[0] + "]");
		var f:Array<Null<Float>> = [Math.NaN];
		var acc = "";
		acc += f[1];
		eq("null", acc);
		eq("NaN", "" + f[0]);
	}
	#end
}
