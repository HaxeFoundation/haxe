package cases;

class Issue7082 extends DisplayTestCase {
	/**
		class Main extends haxe.ds.BalancedTree {
			override {-1-}
			var foo:Int;

			static function main() {}
		}
	**/
	function test() {
		fields(pos(1));
		eq(true, true); // TODO
	}
}
