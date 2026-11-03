package cases.display;

class StructureCompletion extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var o:{a:Float, b:String} = {{-1-}
			}
		}
	**/
	function testStructureCompletion1(_) {
		eq(true, hasField(fields(1), "a", "Float"));
		eq(true, hasField(fields(1), "b", "String"));
	}

	/**
		class Main {
			static function main() {
				var x, o:{a:Float, b:String} = {{-1-}
			}
		}
	**/
	function testStructureCompletion2(_) {
		eq(true, hasField(fields(1), "a", "Float"));
		eq(true, hasField(fields(1), "b", "String"));
	}

	/**
		class Main {
			static function main() {
				var o:{a:Float, b:String};
				o = {{-1-}
			}
		}
	**/
	function testStructureCompletion3(_) {
		eq(true, hasField(fields(1), "a", "Float"));
		eq(true, hasField(fields(1), "b", "String"));
	}

	/**
		class Main {
			static function main () {
				test({{-1-}
			}

			public static function test(o:{a:Float, b:String}) { }
		}
	**/
	function testStructureCompletion4(_) {
		eq(true, hasField(fields(1), "a", "Float"));
		eq(true, hasField(fields(1), "b", "String"));
	}

	/**
		class Main {
			static function main () {
				test(0, {{-1-}
			}

			public static function test(x, o:{a:Float, b:String}) { }
		}
	**/
	function testStructureCompletion5(_) {
		eq(true, hasField(fields(1), "a", "Float"));
		eq(true, hasField(fields(1), "b", "String"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () {{-1-}
	**/
	function testStructureVsToplevel1(_) {
		eq(true, hasToplevel(toplevel(1), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () { {-1-}
	**/
	function testStructureVsToplevel2(_) {
		eq(true, hasToplevel(toplevel(1), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () {{-1-}
			}
	**/
	function testStructureVsToplevel3(_) {
		eq(true, hasToplevel(toplevel(1), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () { {-1-}
			}
	**/
	function testStructureVsToplevel4(_) {
		eq(true, hasToplevel(toplevel(1), "type", "T"));
	}

	/**
		typedef Foo = {
			var a:Int;
			var b:String;
		}
		class Main {
			static function main () {
				var foo:Foo = {{-1-}

	**/
	function testStructureVsToplevel5(_) {
		var f = fields(1);
		eq(false, hasField(f, "type", "T"));
		eq(true, hasField(f, "a", "Int"));
		eq(true, hasField(f, "b", "String"));
	}

	/**
		typedef Foo = {
			var a:Int;
			var b:String;
		}
		class Main {
			static function main () {
				var foo:Foo = { {-1-}

	**/
	function testStructureVsToplevel6(_) {
		var f = fields(1);
		eq(false, hasField(f, "type", "T"));
		eq(true, hasField(f, "a", "Int"));
		eq(true, hasField(f, "b", "String"));
	}

	/**
		typedef Foo = {
			var a:Int;
			var b:String;
		}
		class Main {
			static function main () {
				var foo:Foo = {{-1-}
				}

	**/
	function testStructureVsToplevel7(_) {
		var f = fields(1);
		eq(false, hasField(f, "type", "T"));
		eq(true, hasField(f, "a", "Int"));
		eq(true, hasField(f, "b", "String"));
	}

	/**
		typedef Foo = {
			var a:Int;
			var b:String;
		}
		class Main {
			static function main () {
				var foo:Foo = { {-1-}
				}

	**/
	function testStructureVsToplevel8(_) {
		var f = fields(1);
		eq(false, hasField(f, "type", "T"));
		eq(true, hasField(f, "a", "Int"));
		eq(true, hasField(f, "b", "String"));
	}

	/**
		typedef Foo = {
			var field1:Int;
			var field2:String;
		}
		class Main {
			public static function test1():Foo return { f{-1-}ie{-2-}
			public static function test2():Foo return { f{-3-}ie{-4-}:
			public static function test3():Foo return { f{-5-}ie{-6-} }
			public static function test4():Foo return { f{-7-}ie{-8-} : }
			public static function test5():Foo return { f{-9-}ie{-10-} : null }
			public static function test6():Foo return { f{-11-}ie{-12-} : null
	**/
	function testStructureVsToplevel9(_) {
		for (i in 1...13) {
			var f = fields(i);
			eq(false, hasField(f, "type", "T"));
			eq(true, hasField(f, "field1", "Int"));
			eq(true, hasField(f, "field2", "String"));
		}
	}

	/**
		typedef Foo = {
			var field1:Int;
			var field2:String;
		}
		class Main {
			static function test1():Foo return { field1: 1, f{-1-}ie{-2-}
			static function test2():Foo return { field1: 1, f{-3-}ie{-4-}:
			static function test3():Foo return { field1: 1, f{-5-}ie{-6-} }
			static function test4():Foo return { field1: 1, f{-7-}ie{-8-} : }
			static function test5():Foo return { field1: 1, f{-9-}ie{-10-} : null }
			static function test6():Foo return { field1: 1, f{-11-}ie{-12-} : null
	**/
	function testStructureVsToplevel10(_) {
		for (i in 1...13) {
			var f = fields(i);
			eq(false, hasField(f, "type", "T"));
			eq(false, hasField(f, "field1", "Int"));
			eq(true, hasField(f, "field2", "String"));
		}
	}
}
