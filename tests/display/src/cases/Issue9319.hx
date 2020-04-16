package cases;

class Issue9319 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				try {} catch(e{-1-}) {}
			}
		}
	**/
	function testCatch_noTypeHint() {
		eq("haxe.Exception", type(pos(1)));
	}
}
