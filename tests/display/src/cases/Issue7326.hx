package cases;

class Issue7326 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some({-1-}v):
					case None:
				}
				Some({-2-});
			}
		}
	**/
	function test() {
		// sigEq(0, [["v:Int"]], signature(pos(1)));
		sigEq(0, [["v:Int"]], signature(pos(1)));
		sigEq(0, [["v:Unknown<0>"]], signature(pos(2)));
	}
}
