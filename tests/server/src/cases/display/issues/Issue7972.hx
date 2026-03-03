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
		Assert.equals("Class<Std>", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);
	}
}
