package cases.issues;

import haxe.display.Diagnostic;

class Issue12995 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue12995/Main.hx"));
		var args = ["-main", "Main"];

		runHaxe(args);
		assertErrorMessage("Class<Foo> has no field bar");

		var res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(1, res.length);
		var diags:Array<Diagnostic<Dynamic>> = cast res[0].diagnostics;
		Assert.isTrue(diags.exists(d -> d.kind == MissingFields));

		runHaxe(args);
		Assert.isFalse(hasMessage("reusing Main"));
		assertErrorMessage("Class<Foo> has no field bar");
	}

	function testUnresolvedIdentifier(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue12995/Unresolved.hx"));
		var args = ["-main", "Main"];

		runHaxe(args);
		assertErrorMessage("Unknown identifier : unresolvedIdentifier");

		var res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(1, res.length);
		var diags:Array<Diagnostic<Dynamic>> = cast res[0].diagnostics;
		Assert.isTrue(diags.exists(d -> d.kind == DKUnresolvedIdentifier));

		runHaxe(args);
		Assert.isFalse(hasMessage("reusing Main"));
		assertErrorMessage("Unknown identifier : unresolvedIdentifier");
	}
}
