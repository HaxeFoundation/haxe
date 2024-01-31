package cases;

class Issue11515 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			static function main () {
				Option.{-1-}
			}
		}
	**/
	function testImport() {
		eq(true, hasField(fields(pos(1)), "None", "haxe.ds.Option<haxe.ds.Option.T>"));
	}

	/**
		class Main {
			static function main () {
				haxe.ds.Option.{-1-}
			}
		}
	**/
	function testFully() {
		eq(true, hasField(fields(pos(1)), "None", "haxe.ds.Option<haxe.ds.Option.T>"));
	}
}
