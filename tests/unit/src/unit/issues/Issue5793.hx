package unit.issues;

class Issue5793 extends Test {
	public function test() {
		function run1() {
			function foo<T>(expected:Array<T>, a:T) {
				eq(expected[0], a);
			}
			foo([2], 2);
		}
		run1();

		function run2() {
			function bar<A>(expected:Array<A>, a:A) {
				function baz<B>(expected:Array<B>, b:B) {
					eq(expected[0], b);
				}
				baz(expected, a);
			}
			bar([42], 42);
		}
		run2();
	}
}
