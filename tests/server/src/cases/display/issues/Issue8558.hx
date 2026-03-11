package cases.display.issues;

class Issue8558 extends DisplayTestCase {
	/**
		class Life {
			public function new () {}
			public var die:Int;
		}

		@:forward abstract Immortal(Life) from Life {
			private var die(get,never):Int;
			function get_die() return 0;
		}

		class Main {
			static function main() {
				var bar:Immortal = new Life();
				bar.{-1-}
			}
		}
	**/
	function testAbstractShadowsForwardedField(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "die" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
