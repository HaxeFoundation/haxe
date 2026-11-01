package cases.display.issues;

class Issue8217 extends DisplayTestCase {
	/**
		class Main {
			static function foo( cmd : String ) {
				switch( cmd ) {
				case "user/login":
					var o = {
						x : 55,
						str : "hello".{-1-}
						z : 66,
					};
				}
			}

		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
