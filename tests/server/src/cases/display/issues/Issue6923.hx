package cases.display.issues;

class Issue6923 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				trace({-1-}"test{-2-}".{-3-}lengt{-4-}h{-5-});
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(5)});
		Assert.equals("Void", parseHover().result.item.type.args.path.typeName);
	}

	/**
		class Main {
			static function main() {
				var a = 1;
				{-1-}a{-2-}+{-3-}0{-4-}.{-5-}1;
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(5)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);
	}
}
