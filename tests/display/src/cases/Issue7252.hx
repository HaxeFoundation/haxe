package cases;

class Issue7252 extends DisplayTestCase {
	/**
	class Main {
		var a : Array<Int>;
		function foo() {
			a.map(function(_) {
				a.concat({-1-})
			});
		}
	}
	**/
	function test() {
		sigEq(0, [["a:Array<Int>"]], signature(pos(1)));
	}
}