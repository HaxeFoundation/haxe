package unit.issues;

class Issue8710 extends unit.Test {
	#if (js || lua)
	@:expose('exposed')
	static var field = 10 + Std.random(1);

	#if (js && js.module == "es")
	// ESM exports are not easily accessible as variables.
	// To circumvent this, dynamically import current module:
	function test(async:utest.Async) {
		js.Lib.dynamicImport(js.Syntax.code("import.meta.url")).then(module -> {
			var actual = module.exposed;
			eq(10, actual);
			async.done();
		}).catchError(e -> {
			assert(Std.string(e));
			async.done();
		});
	}
	#else
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
	#end
}
