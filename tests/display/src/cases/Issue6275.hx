package cases;

class Issue6275 extends DisplayTestCase {
	/**
	import haxe.ds.Option;

	class Main {
		static function main() {
			{-1-}n{-2-}ew Main("foo"){-3-};
		}

		{-4-}function new(s:String) {}{-5-}
	}
	**/
	function test() {
		eq("s : String -> Void", type(pos(2)));
		eq(range(4, 5), position(pos(1)));
		eq(range(1, 3), usage(pos(2))[0]);
	}
}
