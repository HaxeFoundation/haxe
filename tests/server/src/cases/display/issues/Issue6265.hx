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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Void", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(10)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(12)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);
	}
}
