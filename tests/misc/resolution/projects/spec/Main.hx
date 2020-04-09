class Main extends utest.Test {
	function testQualified() {
        Macro.assert("pack.Mod");
        Macro.assert("pack.Mod.Mod");
        Macro.assert("pack.Mod.ModSubType");
        Macro.assert("pack.ModNoMain.ModNoMainSubType");
        Macro.assert("pack.ModNoValue.ModNoValueSubType");
        Macro.assert("pack.ModWithStatic.TheStatic");
    }
    
	function testQualifiedStd() {
        Macro.assert("std.pack.Mod");
        Macro.assert("std.pack.Mod.Mod");
        Macro.assert("std.pack.Mod.ModSubType");
        Macro.assert("std.pack.ModNoMain.ModNoMainSubType");
        Macro.assert("std.pack.ModNoValue.ModNoValueSubType");
        Macro.assert("std.pack.ModWithStatic.TheStatic");
    }
    
	function testQualifiedStdShadowed() {
        var pack = 1;
        Macro.assert("std.pack.Mod");
        Macro.assert("std.pack.Mod.Mod");
        Macro.assert("std.pack.Mod.ModSubType");
        Macro.assert("std.pack.ModNoMain.ModNoMainSubType");
        Macro.assert("std.pack.ModNoValue.ModNoValueSubType");
        Macro.assert("std.pack.ModWithStatic.TheStatic");
    }

	static function main() {
		utest.UTest.run([
            new Main(),
            new pack.inner.Test(),
            new Issue9150(),
            new Wildcard(),
            new Imported(),
        ]);
	}
}
