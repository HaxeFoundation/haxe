package pack.inner;

import utest.Assert;

class Test extends utest.Test {
    function testUnqualifiedThisPack() {
        Assert.equals("pack.inner.InnerMod.lowerCase", pack.inner.InnerMod.lowerCase());
        Assert.equals("pack.inner.InnerMod.UpperCase", pack.inner.InnerMod.UpperCase());
    }
    
    function testUnqualifiedUpperPack() {
        Assert.equals("pack.Mod1.lowerCasePack", Mod1.lowerCasePack());
        Assert.equals("pack.Mod1.UpperCasePack", Mod1.UpperCasePack());
    }
    
    function testUnqualifiedRootPack() {
        Assert.equals("RootMod1.lowerCase", RootMod1.lowerCase());
        Assert.equals("RootMod1.UpperCase", RootMod1.UpperCase());
    }
    
    function testUnqualifiedRootPackStd() {
        Assert.equals("RootMod1.lowerCase", std.RootMod1.lowerCase());
        Assert.equals("RootMod1.UpperCase", std.RootMod1.UpperCase());
    }
    
    function testUnqualifiedRootPackStdShadowed() {
        var RootMod = 1;
        Assert.equals("RootMod1.lowerCase", std.RootMod1.lowerCase());
        Assert.equals("RootMod1.UpperCase", std.RootMod1.UpperCase());
    }
}
