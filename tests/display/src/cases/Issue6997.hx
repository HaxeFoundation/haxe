package cases;

class Issue6997 extends DisplayTestCase {
	/**
	"".lastIndexOf.bind({-1-}
	**/
	@:funcCode function test1() {
		sigEq(0, [["str:String", "?startIndex:Null<Int>"]], signature(pos(1)));
	}

	/**
	"".lastIndexOf.bind("foo", {-1-}
	**/
	@:funcCode function test2() {
		sigEq(1, [["str:String", "?startIndex:Null<Int>"]], signature(pos(1)));
	}
}
