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
		final newLineRegex = ~/[\t\n\r]/g;
		s = newLineRegex.replace(s, "");
		eq("class X {final x : Int = 0;var y : Int = 0;private final z : Int = 0;private var w : Int = 0;}", s);

		var td = macro class X {
			static var x(get, never);
		};
		td.fields[0].access.push(AFinal);
		var s = printer.printTypeDefinition(td);
		s = newLineRegex.replace(s, "");
		eq("class X {static final x(get, never);}", s);
	}
}
