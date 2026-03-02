package cases.display.issues;

class Issue11285 extends DisplayTestCase {
	/**
		using Main.MathTools;

		function main() {
			var float = 0.0;
			var int = 0;
			float.wrapA{-1-}round(0, 1);
			int.wrap{-2-}Around(0, 1);
		}

		class MathTools {

			extern overload public static inline function {-3-}wrapAround{-4-}(v:Int, min:Int, max:Int):Int {
				var range = max - min;
				return min + (((v - min) % range) + range) % range;
			}

			extern overload public static inline function {-5-}wrapAround{-6-}(v:Float, min:Float, max:Float):Float {
				var range = max - min;
				return min + (((v - min) % range) + range) % range;
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(5, 6), locs[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);
	}
}
