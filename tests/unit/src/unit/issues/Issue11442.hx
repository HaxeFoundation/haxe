package unit.issues;

import utest.Assert;

enum Dummy {
	DummyCtor;
}

class Issue11442 extends Test {
    function test() {
        f(Std.isOfType(DummyCtor, haxe.ds.Option));
        t(Std.isOfType(DummyCtor, Dummy));

        try {
            throw DummyCtor;
        } catch(e:haxe.ds.Option<Dynamic>) {
            Assert.fail("wrong catch");
        } catch(e:Dummy) {
            Assert.pass("correct catch");
        }
    }
}