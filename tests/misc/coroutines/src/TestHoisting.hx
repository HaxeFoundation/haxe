import haxe.coro.Coroutine.yield;

class TestHoisting extends utest.Test {
    function testLocalVariable() {

        @:coroutine function foo() {
            var bar = 7;

            yield();

            return bar;
        }

        Assert.equals(7, Coroutine.run(foo));
    }

    function testModifyingLocalVariable() {
        @:coroutine function foo() {
            var bar = 7;

            yield();

            bar *= 2;

            yield();

            return bar;
        }

        Assert.equals(14, Coroutine.run(foo));
    }

    @:coroutine function fooTestArgument(v:Int) {
        yield();

        return v;
    }

    function testArgument() {
        Assert.equals(7, Coroutine.run(() -> {
            fooTestArgument(7);
        }));
    }

    @:coroutine function fooTestModifyingArgument(v:Int) {
        yield();

        v *= 2;

        yield();

        return v;
    }

    function testModifyingArgument() {
        Assert.equals(14, Coroutine.run(() -> {
            fooTestModifyingArgument(7);
        }));
    }
}