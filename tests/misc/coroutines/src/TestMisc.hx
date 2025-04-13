import haxe.coro.Coroutine.yield;

class TestMisc extends utest.Test {
    function testDebugMetadataLocalFunction() {
        @:coroutine @:coroutine.debug function foo() {
            yield();
        }

        Coroutine.run(foo);

        Assert.pass();
    }
}