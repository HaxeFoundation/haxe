package unit.issues;

class Issue8710 extends unit.Test {
#if (js || lua)
	@:expose('exposed')
	static var field = 10 + Std.random(1);

	function test() {
		var actual =
			#if js
				js.Syntax.code("$hx_exports[\"exposed\"]");
			#elseif lua
				untyped __lua__("_hx_exports[\"exposed\"]");
			#end
		eq(10, actual);
	}
#end
}