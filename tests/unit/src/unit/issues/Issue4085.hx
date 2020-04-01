package unit.issues;

class Issue4085 extends Test {
	#if js
	function test() {
		var msg = null;
		js.Syntax.code("try { ({0})(); } catch (e) { ({1})(e); }", () -> throw "hello, world", e -> msg = e.message);
		eq(msg, "hello, world");
	}
	#end
}
