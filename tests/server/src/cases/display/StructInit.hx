package cases.display;

class StructInit extends DisplayTestCase {
	/**
		@:structInit
		class Foo {
			var a:Int = null;
			var b:Int;
		}
		class Main {
			static function main() {
				var foo:Foo = {{-1-}
			}
		}
	**/
	function testRequiredAndOptionalFields(_) {
		final fields = fields(1);
		eq(true, hasField(fields, "a", "Null<Int>"));
		eq(true, hasField(fields, "b", "Int"));
	}

	/**
		@:structInit
		class Foo {
			var a:Int = null;
			var b:Int;
		}
		typedef Foo2 = {
			?a:Int,
			b:Int,
		}
		class Main {
			static function main() {
				var foo:Foo = {{-1-}}
				var foo2:Foo2 = {{-2-}
			}
		}
	**/
	function testStructInitVsTypedef() {
		// structInit: optional (a) and required (b) both present
		final classFields = fields(1);
		eq(true, hasField(classFields, "a", "Null<Int>"));
		eq(true, hasField(classFields, "b", "Int"));
		// typedef: same fields also present
		final typedefFields = fields(2);
		eq(true, hasField(typedefFields, "a", "Null<Int>"));
		eq(true, hasField(typedefFields, "b", "Int"));
	}

	/**
		@:structInit
		class Doc {
			/** doctor doc **\/
			public var doctor:Int;
		}
		class Main {
			static function main() {
				final v:Doc = {
					doc{-1-}tor: 1
				};
			}
		}
	**/
	function testFieldHoverDoc() {
		eq("doctor doc", doc(1));
	}

	/**
		@:structInit
		class MultiDoc {
			/** field a **\/
			public var a:Int;
			/** field b **\/
			public var b:String;
		}
		class Main {
			static function main() {
				var v:MultiDoc = {
					a{-1-}: 1,
					b{-2-}: "x"
				};
			}
		}
	**/
	function testMultiFieldHoverDoc() {
		eq("field a", doc(1));
		eq("field b", doc(2));
	}

	/**
		@:structInit
		class Foo {
			var b:Int;
			var a:Int;
			var z:Int;
			var d:Int;
		}
		class Main {
			static function main() {
				var foo:Foo = {{-1-}};
			}
		}
	**/
	function testFieldDeclarationOrder() {
		final fields = fields(1);
		eq(4, fields.length);
		eq(true, isField(fields[0], "b", "Int"));
		eq(true, isField(fields[1], "a", "Int"));
		eq(true, isField(fields[2], "z", "Int"));
		eq(true, isField(fields[3], "d", "Int"));
	}

	/**
		@:structInit
		class Ordered {
			var first:Int;
			var second:String;
			var third:Bool;
		}
		class Main {
			static function main() {
				var v:Ordered = {{-1-}};
			}
		}
	**/
	function testFieldDeclarationOrderMoreTypes() {
		final fields = fields(1);
		eq(3, fields.length);
		eq(true, isField(fields[0], "first", "Int"));
		eq(true, isField(fields[1], "second", "String"));
		eq(true, isField(fields[2], "third", "Bool"));
	}
}
