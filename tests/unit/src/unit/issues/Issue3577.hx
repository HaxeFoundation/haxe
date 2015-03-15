package unit.issues;

class Issue3577 extends Test {
    function test() {
        eq(testNull(2), 4);
    }

    function testNull(?x:Int=0) : Int {
        var y:Int = x;
        function anon() {
            y *= 2;
        }
        anon();
        return y;
    }
}
