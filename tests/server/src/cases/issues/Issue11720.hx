package cases.issues;

import haxe.display.Diagnostic;

class Issue11720 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11720/Main.hx"));
		vfs.putContent("Macro.hx", getTemplate("issues/Issue11720/Macro.hx"));
		vfs.putContent("data/Weapon.hx", getTemplate("issues/Issue11720/data/Weapon.hx"));
		vfs.putContent("col/Collection.hx", getTemplate("issues/Issue11720/col/Collection.hx"));
		vfs.putContent("col/Item.hx", getTemplate("issues/Issue11720/col/Item.hx"));

		var args = ["-main", "Main", "--macro", "include('data',true)", "--interp"];
		runHaxe(args);
		debugErrorMessages();
		assertSuccess();

		runHaxe(args);
		assertSuccess();
		assertHasPrint("Main.hx:4: data.WeaponCollection has been generated");
	}
}
