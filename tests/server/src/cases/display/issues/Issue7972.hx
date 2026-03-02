package cases.display.issues;

class Issue7972 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var foo = 0.0;

				switch (S{-1-}td.i{-2-}nt(fo{-3-}o)) {
					case _:
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Std", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("TFun", parseHover().result.item.type.kind);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);
	}
}
