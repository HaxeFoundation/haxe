package cases;

class Issue8046 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				for(k => v in ['hello' => {field:true}]) {
					k.{-1-}
					v.{-2-}
				}
			}
		}
	**/
	function testKeyValue() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
		eq(true, hasField(fields(pos(2)), "field", "Bool"));
	}
}
