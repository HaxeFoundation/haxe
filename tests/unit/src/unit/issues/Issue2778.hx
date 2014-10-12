package unit.issues;

enum E<T> {
    BoolLit(b:Bool):E<Bool>;
    IntLit(i:Int):E<Int>;
}

class Issue2778 extends Test {

	function test() {
		eq(true, sameType(BoolLit(true), BoolLit(true)));
		eq(false, sameType(BoolLit(false), BoolLit(true)));
		eq(false, sameType(BoolLit(true), BoolLit(false)));
		eq(false, sameType(BoolLit(false), BoolLit(false)));
		eq(12, sameType(IntLit(1), IntLit(11)));
		t(unit.TestType.typeError(sameType(IntLit(1), BoolLit(true))));
		t(unit.TestType.typeError(sameType(BoolLit(true), IntLit(1))));
	}

    static function sameType<S>(o1:E<S>, o2:E<S>):S {
        return switch [o1, o2] {
            case [BoolLit(b1), BoolLit(b2)]: b1 && b2;
            case [IntLit(i1), IntLit(i2)]: i1 + i2;
        }
    }
}