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
		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);

		Assert.equals("Int", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);

		Assert.equals("Int", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);

		Assert.equals("Void", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(5)}).item.type.args.path.typeName);
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
		Assert.equals("Int", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(5)}).item.type.args.path.typeName);
	}
}
