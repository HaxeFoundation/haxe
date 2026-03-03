package cases.display;

private typedef ExpectedSymbol = {
	var name:String;
	var kind:ModuleSymbolKind;
	var ?containerName:String;
}

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
	function testClassFields(_) {
		checkDocumentSymbols([
			{name: "Some", kind: Class, containerName: "Main"},
			{name: "main", kind: Method, containerName: "Main.Some"},
			{name: "x", kind: Field, containerName: "Main.Some"},
			{name: "y", kind: Field, containerName: "Main.Some"},
			{name: "z", kind: Property, containerName: "Main.Some"},
			{name: "new", kind: Constructor, containerName: "Main.Some"}
		], documentSymbols());
	}

	/**
		interface Some {
			function test():Void;
		}
	**/
	function testInterface(_) {
		checkDocumentSymbols([
			{name: "Some", kind: Interface, containerName: "Main"},
			{name: "test", kind: Method, containerName: "Main.Some"}
		], documentSymbols());
	}

	/**
		enum E {
			A;
			B(s:String);
		}
	**/
	function testEnum(_) {
		checkDocumentSymbols([
			{name: "E", kind: Enum, containerName: "Main"},
			{name: "A", kind: EnumMember, containerName: "Main.E"},
			{name: "B", kind: EnumMember, containerName: "Main.E"}
		], documentSymbols());
	}

	/**
		typedef T = {
			x:Int,
		}
	**/
	function testTypedef(_) {
		checkDocumentSymbols([
			{name: "T", kind: Struct, containerName: "Main"},
			{name: "x", kind: Field, containerName: "Main.T"}
		], documentSymbols());
	}

	/**
		abstract A(Int) {
			public function new() { }
			function f() { }
			@:op(A + B) function add(i:Int);
		}
	**/
	function testAbstract(_) {
		checkDocumentSymbols([
			{name: "A", kind: Abstract, containerName: "Main"},
			{name: "new", kind: Constructor, containerName: "Main.A"},
			{name: "f", kind: Method, containerName: "Main.A"},
			{name: "add", kind: Operator, containerName: "Main.A"},
			{name: "i", kind: Variable, containerName: "Main.A.add"}
		], documentSymbols());
	}

	/**
		enum abstract E(Int) {
			static inline var FOO = "test";
			var A;
			@:op(A + B) function add(i:Int);
		}
	**/
	function testEnumAbstract(_) {
		checkDocumentSymbols([
			{name: "E", kind: EnumAbstract, containerName: "Main"},
			{name: "FOO", kind: Constant, containerName: "Main.E"},
			{name: "A", kind: EnumMember, containerName: "Main.E"},
			{name: "add", kind: Operator, containerName: "Main.E"},
			{name: "i", kind: Variable, containerName: "Main.E.add"}
		], documentSymbols());
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
	function testExpression(_) {
		// Note: when the class name matches the module name (class "Main" in "Main.hx"),
		// the module-symbols API returns null for containerName (primary type has no container).
		// Other types in the module get containerName set to the module name "Main".
		checkDocumentSymbols([
			{name: "Main", kind: Class, containerName: null},
			{name: "main", kind: Method, containerName: "Main"},
			{name: "a", kind: Variable, containerName: "Main.main"},
			{name: "b", kind: Variable, containerName: "Main.main"},
			{name: "c", kind: Variable, containerName: "Main.main"},
			{name: "d", kind: Variable, containerName: "Main.main"},
			{name: "e", kind: Variable, containerName: "Main.main"},
			{name: "f", kind: Function, containerName: "Main.main"}
		], documentSymbols());
	}

	/**
		function main() {}
	**/
	function testModuleLevelFields(_) {
		checkDocumentSymbols([{name: "main", kind: Method, containerName: "Main"}], documentSymbols());
	}

	@:coroutine
	function documentSymbols():Array<ModuleSymbol> {
		final result = runHaxeJson(["--no-output"], DisplayMethods.DocumentSymbols, {file: file});
		if (result.length == 0)
			return [];
		return result[0].symbols;
	}

	function checkDocumentSymbols(expected:Array<ExpectedSymbol>, actual:Array<ModuleSymbol>, ?pos:haxe.PosInfos) {
		function toKey(e:ExpectedSymbol) {
			var cn = e.containerName != null ? e.containerName : "";
			return e.kind + ":" + e.name + ":" + cn;
		}
		function toKeyActual(e:ModuleSymbol) {
			var cn = e.containerName != null ? e.containerName : "";
			return e.kind + ":" + e.name + ":" + cn;
		}
		var expectedMap = [for (e in expected) toKey(e) => e];
		for (a in actual) {
			var key = toKeyActual(a);
			Assert.isTrue(expectedMap.exists(key), "Result not part of expected Array: " + key, pos);
			expectedMap.remove(key);
		}
		for (e in expectedMap) {
			Assert.fail("Expected result was not part of actual Array: " + toKey(e), pos);
			return;
		}
	}
}
