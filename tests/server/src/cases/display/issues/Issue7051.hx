package cases.display.issues;

class Issue7051 extends DisplayTestCase {
	/**
		class CWithCtor {
			public function new() { }
		}
		class CInheritedCtor extends CWithCtor { }
		class CInheritedCtor2 extends CInheritedCtor { }
		class CNoCtor { }
		class CNoCtor2 extends CNoCtor { }

		abstract AWithCtor(String) {
			public function new() this = "";
		}
		abstract AWithoutCtor(String) { }

		class Main {
			static function main() {
				new {-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CWithCtor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CInheritedCtor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CInheritedCtor2";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CNoCtor";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CNoCtor2";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "AWithCtor";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "AWithoutCtor";
			case _: false;
		});
	}

	/**
		class Main { static function main() {
			new {-1-}
			call();
		} }
	**/
	function testBroken(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
	}
}
