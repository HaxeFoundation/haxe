package cases.display.issues;

class Issue10704 extends DisplayTestCase {
	/**
		import issue10704.Statics.*;
		class Main {
			static function main() {
				foo{-1-}
			}
		}
	**/
	function test(_) {
		vfs.putContent("issue10704/Statics.hx", "package issue10704;\n\nclass Statics {\n\tpublic static final fooPublic = 0;\n\n\t@:noCompletion\n\tpublic static final fooNoCompletion = 0;\n\n\tstatic final fooPrivate = 0;\n}");
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fooPublic" && item.args.field.scope == Static;
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fooNoCompletion" && item.args.field.scope == Static;
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fooPrivate" && item.args.field.scope == Static;
			case _: false;
		});
	}
}
