package cases;

class Issue5729 extends DisplayTestCase {
	/**
		enum TestEnum {
			Constructor(i:Int);
		}
		class Main {
			public static function main() {
				var c = Constructor(1);
				switch (c) {
					case Constructor(int{-1-}eger): trace("test");
				}
			}
		}
	**/
	function testType1() {
		eq("Int", type(pos(1)));
	}
}
