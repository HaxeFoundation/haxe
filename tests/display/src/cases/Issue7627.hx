package cases;

class Issue7627 extends DisplayTestCase {
	/**

		import haxe.ds.Option;

		class Main {
			public static function main() {
				var option = Some(1);
				option.match(None);
				option.{-1-}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "match", "(this : EnumValue, pattern : Dynamic) -> Bool"));
	}
}
