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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});

		var result = parseHover().result;
		Assert.isNull(result);
	}
}