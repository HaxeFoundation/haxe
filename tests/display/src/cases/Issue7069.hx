package cases;

class Issue7069 extends DisplayTestCase {
	/**
		class Main {
			static var field:Int;
			static function main(argument:Int) {
				var local:Int;
				{
					var blockLocal:Int;
					for (i in 0...10) {
						{-1-}
					}
				}
			}
		}
	**/
	function test() {
		var results = toplevel(pos(1));
		eq(true, isToplevel(results[0], "i"));
		eq(true, isToplevel(results[1], "blockLocal"));
		eq(true, isToplevel(results[2], "local"));
		eq(true, isToplevel(results[3], "argument"));
		eq(true, isToplevel(results[4], "field"));
	}
}
