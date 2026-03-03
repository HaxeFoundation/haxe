package cases.display;

class Toplevel extends DisplayTestCase {
	/**
		class Main {
			static var myField:String;
			static function main() {{-1-}
				{-2-}
	**/
	function testToplevelResuming(_) {
		eq(true, hasToplevel(toplevel(1), "static", "myField"));
		eq(true, hasToplevel(toplevel(2), "static", "myField"));
	}

	/**
		class Main {
			static var myField:String;
			static function main() {
				{-1-}
				var a = "foo";
				{-2-}
	**/
	function testToplevelScoping(_) {
		var toplevel1 = toplevel(1);
		var toplevel2 = toplevel(2);
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
	function testTypeCompletionLocal(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
		eq(false, hasToplevel(typesCompletion, "static", "myField"));
	}

	/**
		class Main {
			var a:{-1-}
	**/
	function testTypeCompletionField(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main {
			static function f(a:{-1-})
	**/
	function testTypeCompletionArgument(_) {
		// TODO: this currently doesn't work if there's no closing paren for function arguments
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Main"));
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main {
			static function main() {
				var a = function(a:{-1-}
			}
		}
	**/
	function testTypeCompletionArgumentLocal(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		import {-1-}
	**/
	function testTypeCompletionImport(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		using {-1-}
	**/
	function testTypeCompletionUsing(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class C0 { }
		class C extends {-1-} {
	**/
	function testTypeCompletionExtends(_) {
		// TODO: this currently doesn't work if there's no token after extends
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "C0"));
	}

	/**
		class C implements {-1-} {
	**/
	function testTypeCompletionImplements(_) {
		eq(true, true); // TODO
	}

	/**
		typedef T = {a:{-1-}}
	**/
	function testTypeCompletionStructureField(_) {
		// TODO: this currently doesn't work if there's no token after the completion position
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main {
			static function main() {
				var a:{a:{-1-}
			}
		}
	**/
	function testTypeCompletionStructureFieldAsType(_) {
		var typesCompletion = toplevel(1);
		eq(true, hasToplevel(typesCompletion, "type", "Array"));
		eq(true, hasToplevel(typesCompletion, "package", "haxe"));
	}

	/**
		class Main {
			static function main() {
				Xml.parse({-1-}
			}
		}
	**/
	function testIssue5969(_) {
		var typesCompletion = toplevel(1);
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
	function testTypeParameters(_) {
		eq(true, hasToplevel(toplevel(1), "type", "FieldT"));
		eq(false, hasToplevel(toplevel(1), "type", "ClassT"));
		eq(true, hasToplevel(toplevel(2), "type", "ClassT"));
		eq(false, hasToplevel(toplevel(2), "type", "FieldT"));
		eq(true, hasToplevel(toplevel(2), "type", "FieldT2"));
	}

	/**
		class Main {
			static function main() {
				{-1-}
			}
			@:noCompletion static function test() { }
		}
	**/
	function testIssue6407(_) {
		eq(false, hasToplevel(toplevel(1), "static", "test"));
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
	function testThisSuper(_) {
		eq(true, hasToplevel(toplevel(1), "literal", "this"));
		eq(false, hasToplevel(toplevel(1), "literal", "super"));

		eq(true, hasToplevel(toplevel(2), "literal", "this"));
		eq(true, hasToplevel(toplevel(2), "literal", "super"));
	}

	/**
		class Main {
			static function f(t:Type.ValueType) {}

			public static function main() {
				f({-1-}
	**/
	function testExpectedType1(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType = {-1-}
	**/
	function testExpectedType2(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType;
				x = {-1-}
	**/
	function testExpectedType3(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:{v:Type.ValueType} = {v: {-1-}};
	**/
	function testExpectedType4(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [{-1-}];
	**/
	function testExpectedType5(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [{-1-}
	**/
	function testExpectedType6(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [null, {-1-}
	**/
	function testExpectedType7(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Array<Type.ValueType> = [null, {-1-} ]
	**/
	function testExpectedType8(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType = {-1-}

				trace("ok");
	**/
	function testExpectedType9(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			public static function main() {
				var x:Type.ValueType;
				x = {-1-}

				trace("ok");
	**/
	function testExpectedType10(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "enum", "TNull"));
	}

	/**
		class Main {
			static function main() {
				for (foo in 0...10){-1-}
					{-2-}
			}
		}
	**/
	function testBokenAST1(_) {
		var f = toplevel(1);
		eq(true, hasToplevel(f, "local", "foo"));
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
	function testTypeParameterApplication(_) {
		var top = toplevel(1);
		eq(true, hasToplevel(top, "member", "f1", "(t : String) -> Void"));
	}
}
