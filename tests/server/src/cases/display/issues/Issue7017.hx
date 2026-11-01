package cases.display.issues;

class Issue7017 extends DisplayTestCase {
	/**
		class Main {
			static public function main() { }

			#if macro static function init() {-1-}"foo"; #end
		}
	**/
	function testMacroInitType(_) {
		eq("String", printer.printType(runHaxeJson(["--macro", "Main.init()"], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type));
	}
}
