package cases.display.issues;

class Issue6275 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			static function main() {
				{-1-}n{-2-}ew Main("foo"){-3-};
			}

			function {-4-}new{-5-}(s:String) {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(4, 5), locs[0].range);
	}
}
