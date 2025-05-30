import hxcoro.Coro.*;

class TestMisc extends utest.Test {
    function testDebugMetadataLocalFunction() {
        @:coroutine @:coroutine.debgu function foo() {
            yield();
        }

        CoroRun.run(foo);

        Assert.pass();
    }
}