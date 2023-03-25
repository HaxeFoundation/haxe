package cases;

class Issue9841 extends DisplayTestCase {
	/**
		typedef Sprite = {
			?x:Float,
			?y:Float,
			?scale:Float,
		}

		class Main {
			static function main() {
				addSprite(sprite -> {
					sprite.{-1-} = 5;
				});
			}

			static function addSprite(s:(sprite:Sprite) -> Void):Void {}
		}
	**/
	function test() {
		var fields = fields(pos(1));
		eq(true, hasField(fields, "x", "Null<Float>", "var"));
		eq(true, hasField(fields, "y", "Null<Float>", "var"));
		eq(true, hasField(fields, "scale", "Null<Float>", "var"));
	}
}
