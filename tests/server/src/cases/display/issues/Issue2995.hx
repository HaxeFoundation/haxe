package cases.display.issues;

class Issue2995 extends DisplayTestCase {
	/**
		class Main {
		    function {-2-}new{-3-}() {}

		    static function main() {
		        var m = {-1-}new Main();
		    }

			static function something() {
		        new Main();
			}
		}
	**/
	function testConstructor(_) {
		Assert.same(range(2, 3), position(1));
		var args = ["--no-output", "-main", "Main"];
		runHaxe(args);
		runHaxeJson(args, DisplayMethods.FindReferences, {file: file, offset: offset(2)});
		var refs = parseGotoDefintion().result;
		Assert.isTrue(refs.length >= 2);
	}
}
