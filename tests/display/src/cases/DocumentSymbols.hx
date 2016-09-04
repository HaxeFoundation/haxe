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
			{ name: "Some", kind: MClass, containerName: null },
			{ name: "main", kind: MMethod, containerName: "Some" },
			{ name: "x", kind: MField, containerName: "Some" },
			{ name: "y", kind: MField, containerName: "Some" },
			{ name: "z", kind: MProperty, containerName: "Some" },
			{ name: "new", kind: MConstructor, containerName: "Some" }
		], ctx.documentSymbols());
	}

	/**
	interface Some {
		function test():Void;
	}
	**/
	function testInterface() {
		checkDocumentSymbols([
			{ name: "Some", kind: MInterface, containerName: null },
			{ name: "test", kind: MMethod, containerName: "Some" }
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
			{ name: "E", kind: MEnum, containerName: null },
			{ name: "A", kind: MMethod, containerName: "E" },
			{ name: "B", kind: MMethod, containerName: "E" }
		], ctx.documentSymbols());
	}

	/**
	typedef T = {
		x:Int,
	}
	**/
	function testTypedef() {
		checkDocumentSymbols([
			{ name: "T", kind: MTypedef, containerName: null },
			{ name: "x", kind: MField, containerName: "T" }
		], ctx.documentSymbols());
	}

	/**
	abstract A(Int) {
		public function new() { }
		function f() { }
	}
	**/
	function testAbstract() {
		checkDocumentSymbols([
			{ name: "A", kind: MAbstract, containerName: null },
			{ name: "new", kind: MConstructor, containerName: "A" },
			{ name: "f", kind: MMethod, containerName: "A" }
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
			{ name: "Main", kind: MClass, containerName: null },
			{ name: "main", kind: MMethod, containerName: "Main" },
			{ name: "a", kind: MVariable, containerName: "Main.main" },
			{ name: "b", kind: MVariable, containerName: "Main.main" },
			{ name: "c", kind: MVariable, containerName: "Main.main" },
			{ name: "d", kind: MVariable, containerName: "Main.main" },
			{ name: "e", kind: MVariable, containerName: "Main.main" },
			{ name: "f", kind: MFunction, containerName: "Main.main" }
		], ctx.documentSymbols());
	}

	function checkDocumentSymbols(expected:Array<ModuleSymbolEntry>, actual:Array<ModuleSymbolEntry>, ?pos:haxe.PosInfos) {
		arrayCheck(expected, actual, function(entry) return entry.kind + ":" + entry.name + ":" + entry.containerName, pos);
	}
}