package cases;

class Issue9077 extends DisplayTestCase {
	/**
		class Main {
			macro static function m(shouldContain:Bool):haxe.macro.Expr {
				var cls = haxe.macro.Context.getLocalClass();
				var pos = shouldContain && cls != null ? cls.get().pos : haxe.macro.Context.currentPos();
				if(haxe.macro.Context.containsDisplayPosition(pos)) {
					return macro 'contains';
				} else {
					return macro false;
				}
			}

			static function main() {
				var str = m(true);
				st{-1-}r;
				var str = m(false);
				st{-2-}r;
			}
		}
	**/
	function test() {
		eq("String", type(pos(1)));
		eq("Bool", type(pos(2)));
	}
}
