package cases;

class DocumentSymbols extends DisplayTestCase {
	/**
		class Some {
			function main() { }
			static var x:String;
			var y:Int;
			var z(default, null):Bool;
			function new() { }
		}
	**/
	function testClassFields() {
		checkDocumentSymbols([
			{name: "Some", kind: Class, containerName: null},
			{name: "main", kind: Method, containerName: "Some"},
			{name: "x", kind: Field, containerName: "Some"},
			{name: "y", kind: Field, containerName: "Some"},
			{name: "z", kind: Property, containerName: "Some"},
			{name: "new", kind: Constructor, containerName: "Some"}
		], ctx.documentSymbols());
	}

	/**
		interface Some {
			function test():Void;
		}
	**/
	function testInterface() {
		checkDocumentSymbols([
			{name: "Some", kind: Interface, containerName: null},
			{name: "test", kind: Method, containerName: "Some"}
		], ctx.documentSymbols());
	}

	/**
		enum E {
			A;
			B(s:String);
		}
	**/
	function testEnum() {
		checkDocumentSymbols([
			{name: "E", kind: Enum, containerName: null},
			{name: "A", kind: EnumMember, containerName: "E"},
			{name: "B", kind: EnumMember, containerName: "E"}
		], ctx.documentSymbols());
	}

	/**
		typedef T = {
			x:Int,
		}
	**/
	function testTypedef() {
		checkDocumentSymbols([
			{name: "T", kind: Struct, containerName: null},
			{name: "x", kind: Field, containerName: "T"}
		], ctx.documentSymbols());
	}

	/**
		abstract A(Int) {
			public function new() { }
			function f() { }
			@:op(A + B) function add(i:Int);
		}
	**/
	function testAbstract() {
		checkDocumentSymbols([
			{name: "A", kind: Abstract, containerName: null},
			{name: "new", kind: Constructor, containerName: "A"},
			{name: "f", kind: Method, containerName: "A"},
			{name: "add", kind: Operator, containerName: "A"},
			{name: "i", kind: Variable, containerName: "A.add"}
		], ctx.documentSymbols());
	}

	/**
		enum abstract E(Int) {
			static inline var FOO = "test";
			var A;
			@:op(A + B) function add(i:Int);
		}
	**/
	function testEnumAbstract() {
		checkDocumentSymbols([
			{name: "E", kind: EnumAbstract, containerName: null},
			{name: "FOO", kind: Constant, containerName: "E"},
			{name: "A", kind: EnumMember, containerName: "E"},
			{name: "add", kind: Operator, containerName: "E"},
			{name: "i", kind: Variable, containerName: "E.add"}
		], ctx.documentSymbols());
	}

	/**
		class Main {
			static function main() {
				var a = 12;
				var b, c = 13;
				var d = 1, e;
				function f() { }
			}
		}
	**/
	function testExpression() {
		checkDocumentSymbols([
			{name: "Main", kind: Class, containerName: null},
			{name: "main", kind: Method, containerName: "Main"},
			{name: "a", kind: Variable, containerName: "Main.main"},
			{name: "b", kind: Variable, containerName: "Main.main"},
			{name: "c", kind: Variable, containerName: "Main.main"},
			{name: "d", kind: Variable, containerName: "Main.main"},
			{name: "e", kind: Variable, containerName: "Main.main"},
			{name: "f", kind: Function, containerName: "Main.main"}
		], ctx.documentSymbols());
	}

	/**
		function main() {}
	**/
	function testModuleLevelFields() {
		checkDocumentSymbols([{name: "main", kind: Method, containerName: null},], ctx.documentSymbols());
	}

	/*// Test
		function main() {}
	 */
	function testLeadingLineComment() {
		checkDocumentSymbols([{name: "main", kind: Method, containerName: null},], ctx.documentSymbols());
	}

	function checkDocumentSymbols(expected:Array<ModuleSymbolEntry>, actual:Array<ModuleSymbolEntry>, ?pos:haxe.PosInfos) {
		for (entry in expected) {
			entry.containerName = "cases.DocumentSymbols" + if (entry.containerName == null) {
				"";
			} else {
				"." + entry.containerName;
			}
		}
		arrayCheck(expected, actual, entry -> entry.kind + ":" + entry.name + ":" + entry.containerName, pos);
	}
}
