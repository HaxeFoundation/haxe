package cases.display.issues;

import haxe.display.Protocol;

class Issue8992 extends DisplayTestCase {
	/**
		class Main {
			static func{-1-}tion main() {
			}
		}
	**/
	function test(_) {
		runHaxe(["--no-output", "-main", "Main"]);
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isNull(result);
	}
}