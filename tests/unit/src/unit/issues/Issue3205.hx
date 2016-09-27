package unit.issues;

class Issue3205 extends Test {
	function test() {
		var td = macro class X {
			public function test1() return 1;
			public function test2() {
				return 1;
			}
			function test3();
		}
		var printer = new haxe.macro.Printer();
		var s = printer.printTypeDefinition(td);
		s = ~/[\t\n\r]/g.replace(s, "");
		eq("class X {public function test1() return 1;public function test2() {return 1;}function test3();}", s);
	}
}