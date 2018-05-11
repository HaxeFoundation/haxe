package cases;

class Issue6265 extends DisplayTestCase {
	/**
	class Main {
		public static function main(x:Int):Void {
			{-1-}trac{-2-}e{-3-}({-4-}'{-5-}${-6-}{{-7-}mai{-8-}n{-9-}({-10-}1{-11-}){-12-}}{-13-}') // lol
		}
	}
	**/
	function test() {
		eq("value : Dynamic -> Void", type(pos(1)));
		eq("value : Dynamic -> Void", type(pos(2)));
		eq("Void", type(pos(3)));
		eq("String", type(pos(4)));
		eq("String", type(pos(5)));
		eq("String", type(pos(6)));
		eq("x : Int -> Void", type(pos(7)));
		eq("x : Int -> Void", type(pos(8)));
		eq("Void", type(pos(9)));
		eq("Int", type(pos(10)));
		eq("Void", type(pos(11)));
		eq("String", type(pos(12)));
		eq("String", type(pos(13)));
	}
}