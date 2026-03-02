package cases.display.issues;

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
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Bool", parseHover().result.item.type.args.path.typeName);
	}
}
