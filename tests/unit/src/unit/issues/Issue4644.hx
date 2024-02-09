package unit.issues;

class Issue4644 extends Test {
#if js

	function test() {
		var isHaxeError = true;
		js.Syntax.code(
			"try {{
				({0})();
			}} catch (e) {{
				({1})(e instanceof {2});
			}}",
			() -> throw (new js.lib.Error():Dynamic),
			b -> isHaxeError = b,
			#if js_unflatten
			js.Syntax.code("haxe.Exception")
			#else
			js.Syntax.code("haxe_Exception")
			#end
		);
		f(isHaxeError);
	}
#end
}
