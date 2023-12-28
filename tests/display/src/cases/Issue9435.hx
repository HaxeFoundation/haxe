package cases;

class Issue9435 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var i:Int;
				foo(i, {-1-})
			}

			static function foo(arg:Int) {}
		}
	**/
	function testCatch_noTypeHint() {
		var fields = toplevel(pos(1));
		eq(true, hasToplevel(fields, "local", "i"));
	}
}
