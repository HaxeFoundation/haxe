package cases;

class Issue7023 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
			inl{-1-}ine funct{-2-}ion fo{-3-}o() return "foo";
		}
	}
	**/
	function test() {
		eq("Void -> String", type(pos(1)));
		eq("Void -> String", type(pos(2)));
		eq("Void -> String", type(pos(3)));
	}
}
