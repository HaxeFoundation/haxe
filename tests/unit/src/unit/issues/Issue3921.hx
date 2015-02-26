package unit.issues;

class Issue3921 extends Test {
    function test() {
        eq(mb(), 3);
    }

    macro static function mb() {
        return macro {
            @:mergeBlock {
                var v = 1;
            }
            v += 2;
            v;
        }
    }
}
