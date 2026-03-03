package cases.display.issues;

class Issue7017 extends DisplayTestCase {
	/**
		class Main {
			static public function main() { }

			#if macro static function init() {-1-}"foo"; #end
		}
	**/
	function testMacroInitType(_) {
		runHaxeJson(["--macro", "Main.init()"], DisplayMethods.Hover, {file: file, offset: offset(1)});
		eq("String", printer.printType(parseHover().result.item.type));
	}
}
