package cases;

class Issue5712 extends DisplayTestCase {
	/**
	typedef Struct = {
		{-1-}field:Float{-2-}
	}
	class Main {
		public static function main() {
			var s:Struct = { fi{-3-}eld: 0 };
			s.fi{-4-}eld;
		}
	}
	**/
	function testType1() {
		eq(range(1, 2), position(pos(3)));
		eq(range(1, 2), position(pos(4)));
		eq("Float", type(pos(3))); // not sure about this one, maybe it should be Int...
		eq("Float", type(pos(4)));
	}
}