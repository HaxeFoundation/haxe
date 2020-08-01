package cases;

class Issue7102 extends DisplayTestCase {
	/**
		import haxe.Constraints.Constructible;
		class Main {
			@:generic static function main<T, TConstructible:Constructible<()->Void>>() {
				new TConstructible({-1-});
			}
		}
	**/
	function test() {
		eq(0, signature(pos(1)).signatures[0].parameters.length);
	}
}
