import pack.inner.*;
import pack.shadow.*;

import utest.Assert;

class Wildcard extends utest.Test {
    function test() {
        Macro.assert("InnerMod");
        Macro.assert("InnerMod.InnerModSubType");
        Assert.isFalse(Macro.resolves("InnerModSubType"));
        Assert.isFalse(Macro.resolves("InnerModNoMainSubType"));
        Assert.isTrue(Macro.resolves("InnerModNoValue"));
        Assert.isFalse(Macro.resolves("InnerModNoValueSubType"));
        Assert.equals(42, Test.f());
    }
}
