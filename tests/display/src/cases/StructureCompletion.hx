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
}