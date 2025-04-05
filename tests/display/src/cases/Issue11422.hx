package cases;

class Issue11422 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var string = "";
				foo(0.0, s{-1-});
			}

			static function foo(a:Int, name:String):Void {}
		}
	**/
	function test() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "string"));
	}

	/**
		class Main {
			static function main() {
				var string = "";
				foo(0.0, s{-1-});
			}

			overload static function foo(a:Int, name:String):Void {}
			overload static function foo(a:Bool, name:String):Void {}
		}
	**/
	function testOverload() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "string"));
	}
}
