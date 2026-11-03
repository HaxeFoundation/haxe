package cases.display.issues;

class Issue6381 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				switch (Some(Some("foo"))) {
					case Some({-1-}in{-2-}ner{-3-} = Some(_)):
						{-4-}inner{-5-};
					case _:
				}
			}
		}
	**/
	function test(_) {
		Assert.equals("Option", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);

		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);
	}
}
