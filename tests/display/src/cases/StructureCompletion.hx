package cases;

class StructureCompletion extends DisplayTestCase {
	/**
		var o:{a:Float, b:String} = {{-1-}
	**/
	@:funcCode function testStructureCompletion1() {
		eq(true, hasField(fields(pos(1)), "a", "Float"));
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
		var x, o:{a:Float, b:String} = {{-1-}
	**/
	@:funcCode function testStructureCompletion2() {
		eq(true, hasField(fields(pos(1)), "a", "Float"));
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
		var o:{a:Float, b:String};
		o = {{-1-}
	**/
	@:funcCode function testStructureCompletion3() {
		eq(true, hasField(fields(pos(1)), "a", "Float"));
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
		class Main {
			static function main () {
				test({{-1-}
			}

			public static function test(o:{a:Float, b:String}) { }
		}
	**/
	function testStructureCompletion4() {
		eq(true, hasField(fields(pos(1)), "a", "Float"));
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
		class Main {
			static function main () {
				test(0, {{-1-}
			}

			public static function test(x, o:{a:Float, b:String}) { }
		}
	**/
	function testStructureCompletion5() {
		eq(true, hasField(fields(pos(1)), "a", "Float"));
		eq(true, hasField(fields(pos(1)), "b", "String"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () {{-1-}
	**/
	function testStructureVsToplevel1() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () { {-1-}
	**/
	function testStructureVsToplevel2() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () {{-1-}
			}
	**/
	function testStructureVsToplevel3() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "T"));
	}

	/**
		typedef T = Dynamic;
		class Main {
			static function main () { {-1-}
			}
	**/
	function testStructureVsToplevel4() {
		eq(true, hasToplevel(toplevel(pos(1)), "type", "T"));
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
	function testStructureVsToplevel5() {
		var fields = fields(pos(1));
		eq(false, hasField(fields, "type", "T"));
		eq(true, hasField(fields, "a", "Int"));
		eq(true, hasField(fields, "b", "String"));
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
	function testStructureVsToplevel6() {
		var fields = fields(pos(1));
		eq(false, hasField(fields, "type", "T"));
		eq(true, hasField(fields, "a", "Int"));
		eq(true, hasField(fields, "b", "String"));
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
	function testStructureVsToplevel7() {
		var fields = fields(pos(1));
		eq(false, hasField(fields, "type", "T"));
		eq(true, hasField(fields, "a", "Int"));
		eq(true, hasField(fields, "b", "String"));
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
	function testStructureVsToplevel8() {
		var fields = fields(pos(1));
		eq(false, hasField(fields, "type", "T"));
		eq(true, hasField(fields, "a", "Int"));
		eq(true, hasField(fields, "b", "String"));
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
	function testStructureVsToplevel9() {
		for (i in 1...13) {
			var fields = fields(pos(i));
			eq(false, hasField(fields, "type", "T"));
			eq(true, hasField(fields, "field1", "Int"));
			eq(true, hasField(fields, "field2", "String"));
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
	function testStructureVsToplevel10() {
		for (i in 1...13) {
			var fields = fields(pos(i));
			eq(false, hasField(fields, "type", "T"));
			eq(false, hasField(fields, "field1", "Int"));
			eq(true, hasField(fields, "field2", "String"));
		}
	}
}
