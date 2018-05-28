package cases;

class Issue7098 extends DisplayTestCase {
	/**
	import misc.issue7098.Bar;
	class Main {
		public static function main() {
			Bar.foo(Va{-1-}lue);
		}
	}
	**/
	function test() {
		var pos = position(pos(1));
		eq("Bar.hx:4: characters 6-11", pos.substr(-25));
	}
}