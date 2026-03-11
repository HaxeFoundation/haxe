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
		Assert.equals("Class<Std>", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.kind == (cast "TFun" : Dynamic));

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);
	}
}
