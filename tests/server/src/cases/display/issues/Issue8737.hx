package cases.display.issues;

class Issue8737 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				#if macro
				te{-1-}st;
				#else
				te{-2-}st;
				#end
			}

			/** Test doc **\/
			macro static function {-4-}test{-5-}() {
				te{-3-}st;
				return macro null;
			}
		}
	**/
	function test(_) {
		for (i in 1...4) {
			runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			var result = parseHover();
			Assert.isTrue(result.result.item.type.kind == (cast "TFun" : Dynamic));
			Assert.equals("Test doc", StringTools.trim(result.result.item.args.field.doc));

			runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(i)});
			var locs = parseGotoDefintion().result;
			Assert.isTrue(locs != null && locs.length > 0);
			Assert.same(range(4, 5), locs[0].range);
		}
	}

	/**
		class Main {
			static function main() {
				te{-1-}st();
			}

			/** Test doc **\/
			macro static function {-2-}test{-3-}() {
				return macro null;
			}
		}
	**/
	function testSimple(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result.item.type.kind == (cast "TFun" : Dynamic));
		Assert.equals("Test doc", StringTools.trim(result.result.item.args.field.doc));

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);
	}
}
