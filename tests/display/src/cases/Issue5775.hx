package cases;

class Issue5775 extends DisplayTestCase {
	/**
	class Main {
		static function main () {
			var a = [];
			trace('${a.{-1-}}');
		}
	}
	**/
	function testType1() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}