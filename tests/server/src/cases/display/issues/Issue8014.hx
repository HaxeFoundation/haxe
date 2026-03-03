package cases.display.issues;

class Issue8014 extends DisplayTestCase {
	/**
		class Main extends MainLoo{-1-} {
			static function main() {}
		}
	**/
	function testExtends(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "MainLoop";
			case _: false;
		});
	}

	/**
		interface Iiiinterface { }
		class Main implements Iii{-1-} {
			static function main() {}
		}
	**/
	function testImplements(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Iiiinterface";
			case _: false;
		});
	}
}
