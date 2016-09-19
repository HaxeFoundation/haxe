package cases;

class Issue5684 extends DisplayTestCase {
	/**
	abstract Test(String) {
		public function hi(){
			this.{-1-}
		}
	}
	**/
	function testType1() {
		// eq("Int", type(pos(1)));
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}