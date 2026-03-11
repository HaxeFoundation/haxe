package cases.display.issues;

class Issue8014 extends DisplayTestCase {
	/**
		class Main extends MainLoo{-1-} {
			static function main() {}
		}
	**/
	function testExtends(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Iiiinterface";
			case _: false;
		});
	}
}
