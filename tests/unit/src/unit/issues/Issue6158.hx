package unit.issues;

class Issue6158 extends unit.Test {
    public static var fn:Dynamic = function(a:Int, b:Int, c:Int) return '$a,$b,$c';

    function test() {
        eq('1,2,3', fn(1, 2, 3));
    }
}