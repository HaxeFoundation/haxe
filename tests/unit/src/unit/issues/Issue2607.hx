package unit.issues;

class Issue2607 extends unit.Test {

    inline static var CONST:Float = -1;

    function test(v = CONST) {
        eq(v, -1);
		t((v is Float));
    }
}