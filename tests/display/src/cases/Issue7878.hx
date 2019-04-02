package cases;

class Issue7878 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {
			var f:Array<SomethingUnk{-1-}nown>;
		}
		}
	**/
	function test() {
		assert(typeNotFound(type.bind(pos(1)), "SomethingUnknown"));
	}
}
