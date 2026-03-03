package cases.display.issues;

class Issue8789 extends DisplayTestCase {
	/**
		abstract Int8(Int) {
			inline function new(value:Int) {
				this = value;
			}

			inline function pvt() {}

			public function test() {
				var i = new Int8{-1-}(10);
				i.{-2-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Int8";
			case _: false;
		});

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "pvt";
			case _: false;
		});
	}
}
