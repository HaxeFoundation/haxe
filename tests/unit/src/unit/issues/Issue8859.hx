package unit.issues;

class Issue8859 extends Test {
	function test() {
		var td = macro class X {
			final x: Int = 0;
			var y: Int = 0;
			private final z: Int = 0;
			private var w: Int = 0;
		}
		var printer = new haxe.macro.Printer();
		var s = printer.printTypeDefinition(td);
		s = ~/[\t\n\r]/g.replace(s, "");
		eq("class X {final x : Int = 0;var y : Int = 0;private final z : Int = 0;private var w : Int = 0;}", s);
	}
}