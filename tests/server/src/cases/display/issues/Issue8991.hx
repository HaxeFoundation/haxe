package cases.display.issues;

import haxe.display.Server;

class Issue8991 extends DisplayTestCase {
	function test(_) {
		var mainHx = 'class Main {
	static function main() {
		C.inst{-1-}ance;
	}
}';
		var cHx = 'class C {
	public static var instance:Int;
}';
		var mainHx = Marker.extractMarkers(mainHx);
		vfs.putContent("Main.hx", mainHx.source);
		vfs.putContent("C.hx", cHx);

		runHaxe(["--no-output", "-main", "Main"]);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("C.hx")});
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: mainHx.markers[1]});

		var result = parseHover().result;
		Assert.equals(DisplayItemKind.ClassField, result.item.kind);
	}
}