package cases.display.issues;

class Issue9039 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("I.hx", "interface I { var prop(get,never):Int; }");
		vfs.putContent("Main.hx", "class Main { static function main() { var i:I = null; } }");

		runHaxe(["--no-output", "-main", "Main"]);

		var content = "class Main { static function main() { var i:I = null; i.{-1-} } }";
		var transform = Marker.extractMarkers(content);

		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: transform.markers[1],
			wasAutoTriggered: true
		});

		assertHasNoCompletion(parseCompletion(), function(item) {
			return switch item.kind {
				case ClassField: item.args.field.name == "get_prop";
				case _: false;
			}
		});
	}
}