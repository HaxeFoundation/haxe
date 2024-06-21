package cases.issues;

import haxe.display.Diagnostic;

class Issue11701 extends TestCase {
	function test(_) {
		vfs.putContent("compile.hxml", getTemplate("issues/Issue11701/compile.hxml"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue11701/Main.hx"));
		vfs.putContent("bar/Bar.hx", getTemplate("issues/Issue11701/Bar.hx"));
		vfs.putContent("baz/Baz.hx", getTemplate("issues/Issue11701/Baz.hx"));

		var content = getTemplate("issues/Issue11701/Foo.hx");
		var transform = Markers.parse(content);
		vfs.putContent("foo/Foo.hx", transform.source);

		var args = ["compile.hxml"];

		runHaxe(args);
		assertSuccess();

		runHaxeJson(args, ServerMethods.Invalidate, {file: new FsPath("foo/Foo.hx")});
		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("foo/Foo.hx"), offset: transform.offset(1)}, _ -> {});
		assertSuccess();
	}
}
