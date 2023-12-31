package cases;

class Issue4155 extends DisplayTestCase {
	/**
		class Main {
			macro static function m(e) {
				trace(haxe.macro.ExprTools.toString(e));
				return switch (e) {
					case macro (null:{
						function dummy():Void {
							$expr;
						}
					}):
						expr;
					case _: throw "invalid input";
				}
			}

			@:debug.display
			static function main() {
				var v = m((null:{
					function dummy():Void {
						"foo".{-1-}
					}
				}));
				trace(v);
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
