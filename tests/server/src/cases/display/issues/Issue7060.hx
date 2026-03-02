package cases.display.issues;

class Issue7060 extends DisplayTestCase {
	/**
		import Type.Valu{-1-}eType;

		class Main {
			static function main() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("ValueType", result.result.item.type.args.path.typeName);
	}
}
