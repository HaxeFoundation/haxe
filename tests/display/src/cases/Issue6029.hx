package cases;

class Issue6029 extends DisplayTestCase {
	/**
		typedef A = {}
		typedef B = {}

		typedef C = {
			>A{-1-},
			>B{-2-},
		}
	**/
	function test() {
		eq("cases.A", type(pos(1)));
		eq("cases.B", type(pos(2)));
	}
}
