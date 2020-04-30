package cases.display.issues;

import haxe.display.Server;
import haxe.display.Protocol;

class Issue9082 extends DisplayTestCase {
	function test(_) {
		var args = ["-cp", ".", "--interp"];

		vfs.putContent("org/Thing.hx", "package org; class Thing {}");
		vfs.putContent("AThing.hx", "class AThing {}");
		vfs.putContent("ThingB.hx", "class ThingB {}");
		runHaxeJson(args, Methods.Initialize, {maxCompletionItems: 2});
		runHaxeJson(args, ServerMethods.ReadClassPaths, null);

		var markers = Markers.parse("class C extends Thing{-1-}");
		vfs.putContent("C.hx", markers.source);
		runHaxeJson(args, DisplayMethods.Completion, {
			file: new FsPath("C.hx"),
			offset: markers.offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, function(item) {
			return switch item {
				case {kind: Type, args: {path: {pack: ["org"], typeName: "Thing"}}}: true;
				case _: false;
			}
		});
	}
}