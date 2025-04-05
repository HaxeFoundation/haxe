package cases.display.issues;

class Issue9956 extends DisplayTestCase {
	/**
		import Issue9956Types;

		class Main {
			static function main() {
				var c:Child = {{-1-}};
			}
		}
	**/
	function test(_) {
		vfs.putContent("Issue9956Types.hx", getTemplate("Issue9956Types.hx"));
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: false
		});
		var result = parseCompletion();
		assertClassField(result, 'x', field -> {
			Assert.equals('This is x', field.doc);
		});
		assertClassField(result, 'y', field -> {
			Assert.equals('This is y', field.doc);
		});
	}
}