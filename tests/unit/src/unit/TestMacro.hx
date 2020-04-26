package unit;

import unit.HelperMacros.parseAndPrint;

class TestMacro extends Test {
	function testPrinter() {
		parseAndPrint("1");
		parseAndPrint("a[b]");
		parseAndPrint("a + b");
		parseAndPrint("a.b");
		parseAndPrint("(a)");
		parseAndPrint("{ a : b }");
		parseAndPrint("{ \"a\" : b }");
		parseAndPrint("[]");
		parseAndPrint("[a]");
		parseAndPrint("[a, b]");
		parseAndPrint("a()");
		parseAndPrint("a(b)");
		parseAndPrint("a(b, c)");
		parseAndPrint("new A()");
		parseAndPrint("new A(a)");
		parseAndPrint("new A(a, b)");
		parseAndPrint("new A<A>()");
		parseAndPrint("new A<A>(a)");
		parseAndPrint("new A<A>(a, b)");
		parseAndPrint("new A<A, B>()");
		parseAndPrint("new A<A, B>(a)");
		parseAndPrint("new A<A, B>(a, b)");
		parseAndPrint("--a");
		parseAndPrint("++a");
		parseAndPrint("-a");
		parseAndPrint("!a");
		parseAndPrint("~a");
		parseAndPrint("a--");
		parseAndPrint("a++");
		parseAndPrint("a!");
		parseAndPrint("var a");
		parseAndPrint("var a:A");
		parseAndPrint("var a = b");
		parseAndPrint("var a:A = b");
		parseAndPrint("function() { }");
		parseAndPrint("for (a in b) { }");
		parseAndPrint("if (a) b");
		parseAndPrint("if (a) b else c");
		parseAndPrint("while (a) b");
		parseAndPrint("do a while (b)");
		parseAndPrint("return");
		parseAndPrint("return a");
		parseAndPrint("break");
		parseAndPrint("continue");
		parseAndPrint("untyped a");
		parseAndPrint("throw a");
		parseAndPrint("cast a");
		parseAndPrint("cast(a, B)");
		parseAndPrint("a ? b : c");
		// parseAndPrint("(a : B)"); // TODO
		parseAndPrint("@:meta a");
		parseAndPrint("@:meta(a) b");
		parseAndPrint("@:meta(a, b) b");
		// new function syntax
		parseAndPrint("var a:(x:X, y:Y) -> Z");
		parseAndPrint("var a:(X, Y) -> Z");
		parseAndPrint("var a:() -> A");
		parseAndPrint("var a:() -> (() -> A)");
		parseAndPrint("var a:(x:(y:Y) -> Z) -> A");
		// local functions
		parseAndPrint('a -> b');
		parseAndPrint('(a:Int) -> b');
		parseAndPrint('(a, b) -> c');
		parseAndPrint('function(a) return b');
		parseAndPrint('function named(a) return b');
		// special handling of single arguments (don't add parentheses)
		//	types
		printComplexType(macro :X -> Y, "X -> Y");
		printComplexType(macro :(X) -> Y, "(X) -> Y");
		printComplexType(macro :((X)) -> Y, "((X)) -> Y");
		printComplexType(macro :?X -> Y, "?X -> Y");
		printComplexType(macro :(?X) -> Y, "(?X) -> Y");
		//	named
		printComplexType(
			// see #9353
			TFunction( [ TOptional( TNamed('a', macro :Int) ) ], macro :Int),
			"(?a:Int) -> Int"
		);
		printComplexType(macro :(a:X) -> Y, "(a:X) -> Y");
		printComplexType(macro :(?a:X) -> Y, "(?a:X) -> Y");
		printComplexType(macro :((?a:X)) -> Y, "((?a:X)) -> Y");
		// multiple arguments are always wrapped with parentheses
		printComplexType(macro :(X, Y) -> Z, "(X, Y) -> Z");
		printComplexType(macro :X -> Y -> Z, "(X, Y) -> Z");
		printComplexType(macro :(X -> Y) -> Z, "(X -> Y) -> Z");
	}

	static function printComplexType(ct:ComplexType, expected: String) {
		var p = new haxe.macro.Printer();
		var printed = p.printComplexType(ct);
		return eq(expected, printed);
	}
}