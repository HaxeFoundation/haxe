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
            return fooTestArgument(7);
        }));
    }

    function testLocalArgument() {
        Assert.equals(7, Coroutine.run(() -> {
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
        Assert.equals(14, Coroutine.run(() -> {
            return fooTestModifyingArgument(7);
        }));
    }

    function testModifyingLocalArgument() {
        Assert.equals(14, Coroutine.run(() -> {
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

        // Coroutine.run(() -> {
        //     i = 7;
        //     yield();
        //     i *= 2;
        // });

        Assert.equals(14, i);
    }

    // function testMultiHoisting() {
    //     Assert.equals(14, Coroutine.run(() -> {

    //         var i = 0;

    //         @:coroutine function foo() {
    //             yield();

    //             i = 7;
    //         }

    //         foo();

    //         return i * 2;

    //     }));
    // }
}