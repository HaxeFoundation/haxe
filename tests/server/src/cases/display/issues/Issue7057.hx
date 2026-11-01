package cases.display.issues;

class Issue7057 extends DisplayTestCase {
	/**
		import haxe.Constraints.Constructible;

		class Main {
			@:generic static function main<T, TConstructible:Constructible<()->Void>>() {
				new {-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasNoCompletion(result, item -> switch item.kind {
			case TypeParameter: item.args.name == "T";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case TypeParameter: item.args.name == "TConstructible";
			case _: false;
		});
	}
}
