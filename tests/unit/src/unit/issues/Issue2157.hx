package unit.issues;

private abstract Choice<X,Y>(Dynamic) {
    private function new (x:Dynamic) this = x;

    @:from public static function fromX <X,Y>(x:X):Choice<X,Y> return new Choice(x);
    @:from public static function fromY <X,Y>(y:Y):Choice<X,Y> return new Choice(y);
}

private abstract False(Bool) {
    inline function new (x:Bool) this = x;
    @:from public static function fromBool (b:Bool) {
        if (!b) {
            throw "assert " + b + " should be false";
        }
        return new False(b);
    }
}


class Issue2157 extends Test {
	function test() {
		var x : False = true;
		t(unit.TestType.typeError(var z : Choice<False, Int> = true));
	}
}