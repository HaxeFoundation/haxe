import pack.inner.*;
import pack.shadow.*;

import utest.Assert;

class Wildcard extends utest.Test {
    function test() {
        Assert.equals("pack.inner.InnerMod.lowerCase", InnerMod.lowerCase());
        Assert.equals("pack.inner.InnerMod.UpperCase", InnerMod.UpperCase());
        Assert.equals("pack.shadow.Test.lowerCase", Test.lowerCase());
        Assert.equals("pack.shadow.Test.UpperCase", Test.UpperCase());
    }
}
