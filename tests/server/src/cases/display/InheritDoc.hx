package cases.display;

class InheritDoc extends DisplayTestCase {
	/**
		import InheritDocTypes;

		class Main {
			static function main() {
				var c = new Chi{-1-}ld();
				c.te{-2-}st();
				Child.tes{-3-}t2();

				final foo = new Foo();
				foo.te{-4-}st();
				final foo2 = new Foo2();
				foo2.te{-5-}st();
				final foo3 = new Foo3();
				foo3.te{-6-}st();
				final foo3inv = new Foo3Inv();
				foo3inv.te{-7-}st();
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

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(4)
		});
		var result = parseHover();
		Assert.equals(' Foo doc \n GrandParent field doc ', result.result.item.args.field.doc);

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(5)
		});
		var result = parseHover();
		Assert.equals(' Foo doc \n IFoo doc ', result.result.item.args.field.doc);

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(6)
		});
		var result = parseHover();
		Assert.equals(' IFoo doc ', result.result.item.args.field.doc);

		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(7)
		});
		var result = parseHover();
		Assert.equals(' IFoo doc ', result.result.item.args.field.doc);
	}
}
