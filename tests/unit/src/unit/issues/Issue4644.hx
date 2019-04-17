package unit.issues;

class Issue4644 extends Test {
	function test() {
		#if js
		var isHaxeError;
		untyped __js__(
			"try {{
				{0};
			}} catch (e) {{
				{1} = (e instanceof {2});
			}}",
			throw (new js.lib.Error() : Dynamic),
			isHaxeError,
			#if js_unflatten
			__js__("js._Boot.HaxeError")
			#else
			__js__("js__$Boot_HaxeError")
			#end
		);
		f(isHaxeError);
		#end
		noAssert();
	}
}
