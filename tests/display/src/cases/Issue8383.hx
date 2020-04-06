package cases;

class Issue8383 extends DisplayTestCase {
	/**
		class Main {
			static var field(never,default):Int;
			static function main() {
				fi{-1-}eld = 10;
			}
		}
	**/
	function test() {
		eq("Int", type(pos(1)));
	}
}
