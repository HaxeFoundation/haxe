package cases;

class Issue6029 extends DisplayTestCase {
	/**
		typedef A = {}
		typedef B = {}

		typedef C = {
			>{-1-}A,
			>{-2-}B,
		}
	**/
	function test() {
		eq("A", type(pos(1)));
		eq("B", type(pos(2)));
	}
}
