package unit.issues;

class Issue10420 extends Test {
#if js
	var count = 0;

	function func() return this.count;

	function test() {
		var obj = {count : 101};

		eq((func :  Dynamic).apply(obj), obj.count);

		var jsbind = (func :  Dynamic).bind(obj);
		eq(jsbind(), obj.count);

		var jsv = js.Syntax.code("{0}.apply({1})", (func : Dynamic), obj);
		eq(jsv, obj.count);

		// .apply will not works on haxe $bind
		var hxv = js.Syntax.code("{0}.apply({1})", func, obj);
		eq(hxv, this.count);
	}
#end
}
