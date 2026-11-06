package unit.teststd.js;

class TestConsole extends Test {
	#if js
	function testConsole() {
		eq("object", js.Syntax.typeof(js.Console));
		t(js.Console.log != null);
		t(js.Console.error != null);
		var console:js.ConsoleInstance = js.Browser.console;
		t(console.log != null);
	}
	#end
}
