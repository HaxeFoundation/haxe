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
		eq("cases.A", type(pos(1)));
		eq("cases.B", type(pos(2)));
	}
}
