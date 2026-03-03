package cases.issues;

import haxe.display.Diagnostic;
import haxe.display.FsPath;

class Issue9676 extends TestCase {
	function testField(_) {
		vfs.putContent("MainField.hx", getTemplate("issues/Issue9676/MainField.hx"));
		var args = ["-main", "MainField", "--interp"];
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("MainField.hx")});
		Assert.equals(1, res.length);
		Assert.equals(1, res[0].diagnostics.length);
		var d = res[0].diagnostics[0];
		Assert.equals((DKParserError : Int), (d.kind : Int));
		Assert.isTrue((cast d.args : String).contains("final var"));
	}

	function testExpr(_) {
		vfs.putContent("MainExpr.hx", getTemplate("issues/Issue9676/MainExpr.hx"));
		var args = ["-main", "MainExpr", "--interp"];
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("MainExpr.hx")});
		Assert.equals(1, res.length);
		Assert.equals(1, res[0].diagnostics.length);
		var d = res[0].diagnostics[0];
		Assert.equals((DKParserError : Int), (d.kind : Int));
		Assert.isTrue((cast d.args : String).contains("final var"));
	}

	function testMacro(_) {
		vfs.putContent("MainMacro.hx", getTemplate("issues/Issue9676/MainMacro.hx"));
		var args = ["-main", "MainMacro", "--interp"];
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("MainMacro.hx")});
		Assert.equals(1, res.length);
		Assert.equals(1, res[0].diagnostics.length);
		var d = res[0].diagnostics[0];
		Assert.equals((DKParserError : Int), (d.kind : Int));
		Assert.isTrue((cast d.args : String).contains("final var"));
	}
}
