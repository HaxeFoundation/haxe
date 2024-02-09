package pack.inner;

class Test extends utest.Test {
    function testUnqualifiedThisPack() {
        Macro.assert("InnerMod");
        Macro.assert("InnerMod.InnerMod");
        Macro.assert("InnerMod.InnerModSubType");
        Macro.assert("InnerModNoMain.InnerModNoMainSubType");
        Macro.assert("InnerModNoValue.InnerModNoValueSubType");
    }
    
    function testUnqualifiedUpperPack() {
        Macro.assert("Mod");
        Macro.assert("Mod.Mod");
        Macro.assert("Mod.ModSubType");
        Macro.assert("ModNoMain.ModNoMainSubType");
        Macro.assert("ModNoValue.ModNoValueSubType");
        Macro.assert("ModWithStatic.TheStatic");
    }
    
    function testUnqualifiedRootPack() {
        Macro.assert("RootMod");
        Macro.assert("RootMod.RootMod");
        Macro.assert("RootMod.RootModSubType");
        Macro.assert("RootModNoMain.RootModNoMainSubType");
        Macro.assert("RootModNoValue.RootModNoValueSubType");
        Macro.assert("RootModWithStatic.TheStatic");
    }
    
    function testUnqualifiedRootPackStd() {
        Macro.assert("std.RootMod");
        Macro.assert("std.RootMod.RootMod");
        Macro.assert("std.RootMod.RootModSubType");
        Macro.assert("std.RootModNoMain.RootModNoMainSubType");
        Macro.assert("std.RootModNoValue.RootModNoValueSubType");
        Macro.assert("std.RootModWithStatic.TheStatic");
    }
    
    function testUnqualifiedRootPackStdShadowed() {
        var RootMod = 1;
        Macro.assert("std.RootMod");
        Macro.assert("std.RootMod.RootMod");
        Macro.assert("std.RootMod.RootModSubType");
        Macro.assert("std.RootModNoMain.RootModNoMainSubType");
        Macro.assert("std.RootModNoValue.RootModNoValueSubType");
        Macro.assert("std.RootModWithStatic.TheStatic");
    }
}
