package cases;

class Issue8789 extends DisplayTestCase {
	/**
		abstract Int8(Int) {
			inline function new(value:Int) {
				this = value;
			}

			public function test() {
				new Int8{-1-}(10);
			}
		}
	**/
	function test() {
		var r = toplevel(pos(1));
		eq(true, hasToplevel(r, "type", "Int8"));
	}
}
