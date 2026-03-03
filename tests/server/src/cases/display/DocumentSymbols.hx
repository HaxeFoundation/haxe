package cases.display;

private enum abstract ModuleSymbolKind(Int) {
	var Class = 1;
	var Interface;
	var Enum;
	var TypeAlias;
	var Abstract;
	var Field;
	var Property;
	var Method;
	var Constructor;
	var Function;
	var Variable;
	var Struct;
	var EnumAbstract;
	var Operator;
	var EnumMember;
	var Constant;
}

private typedef ModuleSymbolEntry = {
	var name:String;
	var kind:ModuleSymbolKind;
	var ?containerName:String;
	var ?isDeprecated:Bool;
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
	function documentSymbols():Array<ModuleSymbolEntry> {
		runHaxe(["--no-output", "--display", "Main.hx@0@module-symbols"]);
		return haxe.Json.parse(lastResult.stderr)[0].symbols;
	}

	function checkDocumentSymbols(expected:Array<ModuleSymbolEntry>, actual:Array<ModuleSymbolEntry>, ?pos:haxe.PosInfos) {
		function toKey(e:ModuleSymbolEntry) {
			var cn = e.containerName != null ? e.containerName : "";
			return e.kind + ":" + e.name + ":" + cn;
		}
		var expectedMap = [for (e in expected) toKey(e) => e];
		for (a in actual) {
			var key = toKey(a);
			Assert.isTrue(expectedMap.exists(key), "Result not part of expected Array: " + key, pos);
			expectedMap.remove(key);
		}
		for (e in expectedMap) {
			Assert.fail("Expected result was not part of actual Array: " + toKey(e), pos);
			return;
		}
	}
}
