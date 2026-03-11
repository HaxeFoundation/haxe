package cases.display.issues;

class Issue4651 extends DisplayTestCase {
	/**
		some
		    invalid
		    crap
		        :-)
	**/
	function testCompletionWithContents(_) {
		// The file on disk has invalid content, but we pass valid content inline
		final content = "class Main { static function main() { Main.{-1-} } }";
		final transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", source); // put the "invalid" file content
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: transform.markers[1],
			contents: transform.source,
			wasAutoTriggered: false
		});
		final result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "main";
			case _: false;
		});
	}
}
