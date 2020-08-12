package cases.display;

class InheritDoc extends DisplayTestCase {
	/**
		import InheritDocTypes;

		class Main {
			static function main() {
				var c = new Chi{-1-}ld();
				c.te{-2-}st();
				Child.tes{-3-}t2();
			}
		}
	**/
	function test(_) {
		vfs.putContent("InheritDocTypes.hx", getTemplate("InheritDocTypes.hx"));

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(1)
		});
		var result = parseHover();
		Assert.equals(' Child class doc \n GrandParent class doc ', result.result.item.args.doc);

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(2)
		});
		var result = parseHover();
		Assert.equals(' Child field doc \n GrandParent field doc ', result.result.item.args.field.doc);

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(3)
		});
		var result = parseHover();
		Assert.equals(' Child field 2 doc \n unrelated field doc ', result.result.item.args.field.doc);
	}
}