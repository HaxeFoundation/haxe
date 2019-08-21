package cases;

class Issue7133 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var option:Option<Int> = if (true) No{-1-}ne else
			}
		}
	**/
	function test() {
		eq(true, position(pos(1)) != null);
	}
}
