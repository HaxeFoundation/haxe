package unit.teststd.js;

class TestConsole extends Test {
	#if js
	function testConsole() {
		eq("object", js.Syntax.typeof(js.Console));
		t(js.Console.log != null);
		t(js.Console.error != null);
	}
	#end
}
