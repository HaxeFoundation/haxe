package cases;

class Abstract extends DisplayTestCase {
	/**
	abstract A(Int) {
		public function new({-3-}i{-4-}) {
			this = {-1-}i;
			trace("f{-2-}oo");
		}
	}
	**/
	function test() {
		eq(range(3, 4), position(pos(1)));
		eq("String", type(pos(2)));
	}
}