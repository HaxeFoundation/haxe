package cases;

class Issue8180 extends DisplayTestCase {
	/**
		class Main {
			#if !macro
			static function main() {
				f({-1-}
			}
			#end

			static macro function f(e) {
				switch e {
					case {expr: EDisplay(macro null, DKMarked), pos: p}:
						return {pos: p, expr: EDisplay(macro {x:1}, DKDot)};
					case _:
						return macro null;
				}
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "x", "Int"));
	}
}
