package cases;

class Issue7752 extends DisplayTestCase {
	/**
		class Foo {
		extern function foo(te{-1-}st:Int):Void;
		}
	**/
	function test() {
		eq("Int", type(pos(1)));
	}

	/**
		class Foo {
		extern function foo(test:Int = 1{-1-}2):Void;
		}
	**/
	function test2() {
		eq("Int", type(pos(1)));
	}
}
