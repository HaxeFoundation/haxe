package issues.aidan;

import utest.Assert;
import haxe.coro.Coroutine;
import haxe.coro.Coroutine.yield;

private interface IFoo {
    @:coroutine function bar():Void;
}

private class Foo implements IFoo {
    public function new() {}

    @:coroutine public function bar() {
        yield();
    }
}

class Issue61 extends utest.Test {
    public function test() {
        Coroutine.run(() -> {
            final f : IFoo = new Foo();
    
            f.bar();
        });
    }
}