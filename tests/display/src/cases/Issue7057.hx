package cases;

class Issue7057 extends DisplayTestCase {
	/**
		import haxe.Constraints.Constructible;

		class Main {
			@:generic static function main<T, TConstructible:Constructible<()->Void>>() {
				new {-1-}
			}
		}
	**/
	function test() {
		eq(false, hasToplevel(toplevel(pos(1)), "type", "T"));
		eq(true, hasToplevel(toplevel(pos(1)), "type", "TConstructible"));
	}
}
