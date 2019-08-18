package cases;

class Issue7319 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			static function main() {
				var option:Option<Int>;
				switch option {
					case None:
					case So{-1-}
				}
			}
		}
	**/
	function test() {
		var fields = toplevel(pos(1));
		eq(false, hasField(fields, "So", "haxe.ds.Option<Int>"));
	}
}
