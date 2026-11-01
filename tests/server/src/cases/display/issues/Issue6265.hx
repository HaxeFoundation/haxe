package cases.display.issues;

class Issue6265 extends DisplayTestCase {
	/**
		class Main {
			public static function main(x:Int):Void {
				{-1-}trac{-2-}e{-3-}({-4-}'{-5-}${-6-}{{-7-}mai{-8-}n{-9-}({-10-}1{-11-}){-12-}}{-13-}') // lol
			}
		}
	**/
	function test(_) {
		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.kind == (cast "TFun" : Dynamic));

		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.kind == (cast "TFun" : Dynamic));

		Assert.equals("Void", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);

		Assert.equals("Int", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(10)}).item.type.args.path.typeName);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(12)}).item.type.args.path.typeName);
	}
}
