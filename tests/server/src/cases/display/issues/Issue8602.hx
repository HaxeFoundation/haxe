package cases.display.issues;

class Issue8602 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				haxe.ds.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: true});
		var result = parseCompletion();
		Assert.equals(Toplevel, result.result.mode.kind);
	}
}