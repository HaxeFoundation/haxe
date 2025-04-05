package cases;

class Issue11285 extends DisplayTestCase {
	/**
		using Issue11285.MathTools;

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
	function test() {
		eq(range(5, 6), position(pos(1)));
		eq("(v : Float, min : Float, max : Float) -> Float", type(pos(1)));

		eq(range(3, 4), position(pos(2)));
		eq("(v : Int, min : Int, max : Int) -> Int", type(pos(2)));
	}
}
