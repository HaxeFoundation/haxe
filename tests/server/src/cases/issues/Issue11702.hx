package cases.issues;

import haxe.display.Diagnostic;

class Issue11702 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11702/Main.hx"));
		vfs.putContent("Foo.hx", getTemplate("issues/Issue11702/Foo.hx"));
		vfs.putContent("Bar.hx", getTemplate("issues/Issue11702/Bar.hx"));
		vfs.putContent("Proxy.hx", getTemplate("issues/Issue11702/Proxy.hx"));
		vfs.putContent("State.hx", getTemplate("issues/Issue11702/State.hx"));
		vfs.putContent("Macro.macro.hx", getTemplate("issues/Issue11702/Macro.hx"));

		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();

		var content = getTemplate("issues/Issue11702/Foo2.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Foo.hx", transform.source);

		var foo = new FsPath("Foo.hx");
		runHaxeJson(args, ServerMethods.Invalidate, {file: foo});

		runHaxeJsonCb(args, DisplayMethods.Hover, {file: foo, offset: transform.offset(1)}, res -> {
			Assert.equals("Main", res.item.args.path.moduleName);
		});
		assertSuccess();
	}
}
