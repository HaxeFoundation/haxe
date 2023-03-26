package cases.display.issues;

class Issue9012 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("Some.hx", "class Some { public static function func():String return 'hello'; }");

		var content = "import Some.func; class Main { static function main() { fu{-1-}nc(); } }";
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		runHaxe(["--no-output", "-main", "Main"]); // commenting this makes it work
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: transform.markers[1]});
		var result = parseHover().result;

		Assert.equals(DisplayItemKind.ClassField, result.item.kind);
	}
}