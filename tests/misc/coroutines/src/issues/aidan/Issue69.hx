package issues.aidan;

import utest.Assert;

private interface IFoo {
    @:coroutine function bar():Void;
}

private class Foo implements IFoo {
    public function new() {}

    @:coroutine public function bar() {
        yield();
    }
}

class Issue69 extends utest.Test {
    public function test() {
        CoroRun.run(() -> {
            final f : IFoo = new Foo();

            f.bar();
        });

        Assert.pass();
    }
}