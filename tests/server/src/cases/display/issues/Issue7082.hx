package cases.display.issues;

class Issue7082 extends DisplayTestCase {
	/**
		class Main extends haxe.ds.BalancedTree {
			override {-1-}
			var foo:Int;

			static function main() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.pass(); // TODO: test override completion
	}
}
