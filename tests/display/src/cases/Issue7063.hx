package cases;

class Issue7063 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				call({
					foo: 1,{-1-}
				});
			}

			static function call(arg1:Dynamic, arg2:Int) { }
		}
	**/
	function test() {
		// assumes wasAutoTriggered = false, update test for new protocol
		sigEq(0, [["arg1:Dynamic", "arg2:Int"]], signature(pos(1)));
	}
}
