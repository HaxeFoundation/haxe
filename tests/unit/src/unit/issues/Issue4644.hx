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
			__js__("haxe.Error")
			#else
			__js__("haxe_Error")
			#end
		);
		f(isHaxeError);
		#end
		noAssert();
	}
}
