import hxcoro.Coro.*;

class TestHoisting extends utest.Test {
    function testLocalVariable() {

        @:coroutine function foo() {
            var bar = 7;

            yield();

            return bar;
        }

        Assert.equals(7, CoroRun.run(foo));
    }

    function testModifyingLocalVariable() {
        @:coroutine function foo() {
            var bar = 7;

            yield();

            bar *= 2;

            yield();

            return bar;
        }

        Assert.equals(14, CoroRun.run(foo));
    }

    @:coroutine function fooTestArgument(v:Int) {
        yield();

        return v;
    }

    function testArgument() {
        Assert.equals(7, CoroRun.run(() -> {
            return fooTestArgument(7);
        }));
    }

    function testLocalArgument() {
        Assert.equals(7, CoroRun.run(() -> {
            @:coroutine function foo(v:Int) {
                yield();

                return v;
            }

            return foo(7);
        }));
    }

    @:coroutine function fooTestModifyingArgument(v:Int) {
        yield();

        v *= 2;

        yield();

        return v;
    }

    function testModifyingArgument() {
        Assert.equals(14, CoroRun.run(() -> {
            return fooTestModifyingArgument(7);
        }));
    }

    function testModifyingLocalArgument() {
        Assert.equals(14, CoroRun.run(() -> {
            @:coroutine function foo(v:Int) {
                yield();

                v *= 2;

                yield();

                return v;
            }

            return foo(7);
        }));
    }

    function testCapturingLocal() {
        var i = 0;

        CoroRun.run(() -> {
            i = 7;
            yield();
            i *= 2;
        });

        Assert.equals(14, i);
    }

    function testMultiHoisting() {
        Assert.equals(14, CoroRun.run(() -> {

            var i = 0;

            @:coroutine function foo() {
                yield();

                i = 7;
            }

            foo();

            return i * 2;

        }));
    }

    function testLoopHoisting() {
        final expected = [1, 2, 3];
        final actual   = [];

        CoroRun.runScoped(node -> {
            for (x in expected) {
                node.async(_ -> {
                    actual.push(x);
                });
            }
        });

        Assert.same(expected, actual);
    }

    function testUninitialisedVariable() {
        Assert.equals(7, CoroRun.run(() -> {
            var i;

            yield();

            i = 7;

            yield();

            return i;
        }));
    }
}