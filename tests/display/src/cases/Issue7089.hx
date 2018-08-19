package cases;

class Issue7089 extends DisplayTestCase {
	/**
	enum abstract Foo(Int) {
		var Value = 1;
	}

	class Main {
		static function main() {
			Fo{-1-}o;
		}
	}
	**/
	function test() {
		eq("Abstract<cases.Foo>", type(pos(1)));
	}
}