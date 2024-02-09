package cases.display.issues;

class Issue7305 extends DisplayTestCase {
	/**
		class Main {
			static public function main() {
				new Map{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch (item.kind) {
			case Type: item.args.path.pack.length == 0 && item.args.path.typeName == "Map";
			case _: false;
		});
	}
}