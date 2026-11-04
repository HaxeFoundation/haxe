package cases.display.issues;

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
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x" && item.args.field.kind.kind == FVar;
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "y" && item.args.field.kind.kind == FVar;
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "scale" && item.args.field.kind.kind == FVar;
			case _: false;
		});
	}
}
