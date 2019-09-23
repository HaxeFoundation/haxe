package cases;

class Issue8789 extends DisplayTestCase {
	/**
		abstract Int8(Int) {
			inline function new(value:Int) {
				this = value;
			}

			inline function pvt() {}

			public function test() {
				var i = new Int8{-1-}(10);
				i.{-2-}
			}
		}
	**/
	function test() {
		var r = toplevel(pos(1));
		eq(true, hasToplevel(r, "type", "Int8"));
		eq(true, hasField(fields(pos(2)), "pvt", "Void -> Void"));
	}
}
