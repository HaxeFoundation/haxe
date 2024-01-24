package cases;

import Types;

using Lambda;

class Toplevel extends DisplayTestCase {
	/**
		class Main {
			static var myField:String;
			static function main() {{-1-}
				{-2-}
	**/
	function testToplevelResuming() {
		eq(true, hasToplevel(toplevel(pos(1)), "static", "myField"));
		eq(true, hasToplevel(toplevel(pos(2)), "static", "myField"));
	}

	/**
		class Main {
			static var myField:String;
			static function main() {
				{-1-}
				var a = "foo";
				{-2-}
	**/
	function testToplevelScoping() {
		var toplevel1 = toplevel(pos(1));
		var toplevel2 = toplevel(pos(2));
		eq(true, hasToplevel(toplevel1, "static", "myField"));
		eq(true, hasToplevel(toplevel2, "static", "myField"));
		eq(false, hasToplevel(toplevel1, "local", "a"));
		eq(true, hasToplevel(toplevel2, "local", "a"));
	}

	/**
		class Main {
			static var myField:String;
			static function main() {
				var a:{-1-}
	**/
	function testTypeCompletionLocal() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
		eq(false, hasToplevel(typesCompletion, "static", "myField"));
	}

	/**
		class Main {
			var a:{-1-}
	**/
	function testTypeCompletionField() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main {
			static function f(a:{-1-})
	**/
	function testTypeCompletionArgument() {
		// TODO: this currently doesn't work if there's no closing paren for function arguments
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		var a = function(a:{-1-}
	**/
	@:funcCode function testTypeCompletionArgumentLocal() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		import {-1-}
	**/
	function testTypeCompletionImport() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		using {-1-}
	**/
	function testTypeCompletionUsing() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class C0 { }
		class C extends {-1-} {
	**/
	function testTypeCompletionExtends() {
		// TODO: this currently doesn't work if there's no token after extends
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "C0"));
		// eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class C implements {-1-} {
	**/
	function testTypeCompletionImplements() {
		// TODO: this currently doesn't work if there's no token after implements
		// NOTE: This test is invalid, we only show interfaces after `implements`
		// var typesCompletion = toplevel(pos(1));
		// eq(true, hasToplevel(typesCompletion, "type", "Array"));
		// eq(true, hasToplevel(typesCompletion, "package", "haxe"));
		eq(true, true); // TODO
	}

	/**
		typedef T = {a:{-1-}}
	**/
	function testTypeCompletionStructureField() {
		// TODO: this currently doesn't work if there's no token after the completion position
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		var a:{a:{-1-}
	**/
	@:funcCode function testTypeCompletionStructureFieldAsType() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		Xml.parse({-1-}
	**/
	@:funcCode function testIssue5969() {
		var typesCompletion = toplevel(pos(1));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main<ClassT> {
			static var myField:String;
			static function main<FieldT>() {
				{-1-}
			}

			function field<FieldT2>() {
				{-2-}
			}
	**/
	function testTypeParameters() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "FieldT"));
		eq(false, hasToplevel(toplevel(pos(1)), "type", "ClassT"));
		eq(true, hasToplevel(toplevel(pos(2)), "type", "ClassT"));
		eq(false, hasToplevel(toplevel(pos(2)), "type", "FieldT"));
		eq(true, hasToplevel(toplevel(pos(2)), "type", "FieldT2"));
	}

	/**
		import cases.Toplevel.E.a;

		enum E {
			a;
		}

		class Main {
			static var a:Int;
			function new(a) {
				{-1-}
			}

			static function main() {
			}
		}
	**/
	function testDuplicates() {
		var toplevels = toplevel(pos(1));
		toplevels = toplevels.filter(t -> isToplevel(t, "a"));
		// TODO: fix that with display/completion
		eq(1, toplevels.length);
		eq(true, isToplevel(toplevels[0], "a", null, "local"));
	}

	/**
		class Main {
			static function main() {
				{-1-}
			}
			@:noCompletion static function test() { }
		}
	**/
	function testIssue6407() {
		eq(false, hasToplevel(toplevel(pos(1)), "static", "test"));
	}

	/**
		class Parent {
			function parent() {
				{-1-}
			}
		}
		class Child extends Parent {
			function child() {
				{-2-}
			}
		}
	**/
	function testThisSuper() {
		eq(true, hasToplevel(toplevel(pos(1)), "literal", "this"));
		eq(false, hasToplevel(toplevel(pos(1)), "literal", "super"));

		eq(true, hasToplevel(toplevel(pos(2)), "literal", "this"));
		eq(true, hasToplevel(toplevel(pos(2)), "literal", "super"));
	}

	/**
		class Main {
			static function f(t:Type.ValueType) {}

			public static function main() {
				f({-1-}
	**/
	function testExpectedType1() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType = {-1-}
	**/
	function testExpectedType2() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType;
				x = {-1-}
	**/
	function testExpectedType3() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:{v:Type.ValueType} = {v: {-1-}};
	**/
	function testExpectedType4() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [{-1-}];
	**/
	function testExpectedType5() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [{-1-}
	**/
	function testExpectedType6() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [null, {-1-}
	**/
	function testExpectedType7() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [null, {-1-} ]
	**/
	function testExpectedType8() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType = {-1-}

				trace("ok");
	**/
	function testExpectedType9() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType;
				x = {-1-}

				trace("ok");
	**/
	function testExpectedType10() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "enum", "TNull"));
	}

	/**
		class Main {
			static function main() {
				for (foo in 0...10){-1-}
					{-2-}
			}
		}
	**/
	function testBokenAST1() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "local", "foo"));
	}

	/**
		class C1<T> {
			public function f1(t:T) { }
		}

		class C2<T> extends C1<T> { }

		class C3 extends C2<String> {
			function f2() {
				{-1-}
			}
		}
	**/
	function testTypeParameterApplication() {
		var toplevel = toplevel(pos(1));
		eq(true, hasToplevel(toplevel, "member", "f1", "(t : String) -> Void"));
	}
}
