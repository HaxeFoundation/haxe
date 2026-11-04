package cases.display;

class VsHaxeIssue198 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				'foo.{-1-}${"foo.{-2-}".{-3-}}';
			}
		}
	**/
	function test(_) {
		eq(true, hasField(fields(3), "length", "Int"));
	}
}
