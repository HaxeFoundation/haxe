package cases;

class Issue5709 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var foo:F{-1-}oo = new F{-2-}oo();
			}
		}

		/** Foo doc **\/
		class Foo {
			public function new() {}
		}

		/** IBaz doc **\/
		interface IBaz {}

		class Bar extends F{-3-}oo {}
		class Baz implements I{-5-}Baz {}

		typedef Foos = Array<F{-4-}oo>;
	**/
	function test() {
		for (i in 1...5) eq("Foo doc", doc(pos(i)));
		eq("IBaz doc", doc(pos(5)));
	}
}
