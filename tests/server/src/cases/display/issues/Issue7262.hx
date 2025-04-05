package cases.display.issues;

class Issue7262 extends DisplayTestCase {
	/**
		class Main {
			static public function main() {
				var x:haxe.extern.EitherType<haxe.PosInfos, () -> Void> = {-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals("TAnonymous", result.mode.args.expectedTypeFollowed.args.params[0].kind);
	}
}