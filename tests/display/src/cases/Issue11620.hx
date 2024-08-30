package cases;

class Issue11620 extends DisplayTestCase {
	/**
		import misc.issue11620.Foo.Bar;

		function main() {
			Bar.bar();
		}
	**/
	function test() {
		arrayEq([], diagnostics());
	}
}
