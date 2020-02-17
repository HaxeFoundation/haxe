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
		eq("i", results[0].name);
		eq("blockLocal", results[1].name);
		eq("local", results[2].name);
		eq("argument", results[3].name);
		eq("field", results[4].name);
	}
}
