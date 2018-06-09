package unit.issues;

class Issue6560 extends unit.Test {

    function test() {
        function foo<F>(a:F):Array<F> {
            if (false) foo(1);
            return if (a == null) [] else foo(null);
        }
        var bar:Array<Int> = foo(1);
        eq(0, bar.length);
    }

}