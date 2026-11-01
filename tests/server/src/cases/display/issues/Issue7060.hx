package cases.display.issues;

class Issue7060 extends DisplayTestCase {
	/**
		import Type.Valu{-1-}eType;

		class Main {
			static function main() {}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("ValueType", result.item.type.args.path.typeName);
	}
}
