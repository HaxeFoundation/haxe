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

		static function test(o:{a:Float, b:String}) { }
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

		static function test(x, o:{a:Float, b:String}) { }
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
}