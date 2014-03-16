package unit.issues;
import unit.Test;

@:enum abstract WorldRegion(Int) {
    static public var MiddleCenter = 1;
}

class Issue2633 extends Test {

	function test() {
       eq(1, match(0));
       eq(1, match(1));
       eq(2, match(2));
       eq(2, match(3));
	}

	function match(i) {
		return switch [WorldRegion.MiddleCenter, i] {
            case [WorldRegion.MiddleCenter, 1]|[WorldRegion.MiddleCenter, 0]:
				1;
            case _:
                2;
        }
	}
}