package cases;

class Issue5967 extends DisplayTestCase {
	/**
	function call():Void { };
	call({-1-}a{-2-},{-3-},{-4-} {-5-}
	**/
	@:funcCode function test1() {
		sigEq(0, [[]], signature(pos(1)));
		sigEq(0, [[]], signature(pos(2)));
		sigEq(1, [[]], signature(pos(3)));
		sigEq(2, [[]], signature(pos(4)));
		sigEq(2, [[]], signature(pos(5)));
	}
}
