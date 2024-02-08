package cases;

class Issue8558 extends DisplayTestCase {
	/**
		class Life {
			public function new () {}
			public var die:Int;
		}

		@:forward abstract Immortal(Life) from Life {
			private var die(get,never):Int;
			function get_die() return 0;
		}

		class Main {
			static function main() {
				var bar:Immortal = new Life();
				bar.{-1-}
			}
		}
	**/
	function testAbstractShadowsForwardedField() {
		eq(false, hasField(fields(pos(1)), "die", "Int"));
	}
}
