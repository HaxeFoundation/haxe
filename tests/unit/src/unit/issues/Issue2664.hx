package unit.issues;
import unit.Test;

class Issue2664 extends Test {

    function test() {
        var i = 0;
		var buf = "";
        eq(-2,sub({buf += "a" + i; i++; },toABase({ buf += "b" + i; i++;})));
		eq("a0b1", buf.toString());
		eq(2, i);
    }

    static function sub(a, b) {
        return a - b;
    }

    static function toABase(i) {
        return i * 2;
    }
}