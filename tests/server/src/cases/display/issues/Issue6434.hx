package cases.display.issues;

class Issue6434 extends DisplayTestCase {
	/**
		import ModuleWithPrivateType;

		class Main {
			static function main() {
				{-1-}
			}
		}
	**/
	function test(_) {
		vfs.putContent("ModuleWithPrivateType.hx", "package;\n\nclass PublicClass {}\nprivate class PrivateClass {}");
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PublicClass";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateClass";
			case _: false;
		});
	}
}
