package issues.aidan;

import utest.Assert;
import haxe.coro.Coroutine;
import haxe.coro.Coroutine.yield;

class Issue61 extends utest.Test {
	public function test() {
		Coroutine.run(foo);
	}

    @:coroutine function foo() {
        var a = 2;
        yield();
        Assert.equals(2, a);

        var a = 1;
        yield();
        Assert.equals(1, a);
    }
}