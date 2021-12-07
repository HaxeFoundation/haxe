package cases.display.issues;

class Issue10414 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var a = 1;
				var obj:{
					foo:Int,
					bar:Bool
				} = {
					foo:{-1-}
					bar: true
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(parseCompletion(), item -> item.args.name == 'a');
	}
}