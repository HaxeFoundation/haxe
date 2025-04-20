import haxe.coro.Coroutine.yield;

class TestMisc extends utest.Test {
    function testDebugMetadataLocalFunction() {
        @:coroutine @:coroutine.debgu function foo() {
            yield();
        }

        Coroutine.run(foo);

        Assert.pass();
    }
}