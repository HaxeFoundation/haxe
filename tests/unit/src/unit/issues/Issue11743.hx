package unit.issues;

import utest.Assert;

private interface IFoo {
    function foo():Void;
}

class Issue11743 extends Test {

    #if cpp
    function test() {
        final o : IFoo = null;

        Assert.raises(() -> o.foo());
    }
    #end
}