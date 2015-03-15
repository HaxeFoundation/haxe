package unit.issues;
import unit.Test;


class Issue2610 extends Test {
	function test() {
		function map(i1, i2) {
			return switch [i1, i2] {
				case [1, 1]: 1;
				case [1 | 2, 2 | 1]: 2;
				case _: 3;
			}
		}

		eq(1, map(1, 1));
		eq(2, map(1, 2));
		eq(2, map(2, 1));
		eq(2, map(2, 2));
		eq(3, map(2, 3));

		function map2(i1, i2) {
			return switch [i1, i2] {
				case [1, 1]: "1+1";
				case [a = 1 | 2, b = 2 | 1]: '$a+$b';
				case _: "_";
			}
		}

		eq("1+1", map2(1, 1));
		eq("1+2", map2(1, 2));
		eq("2+1", map2(2, 1));
		eq("2+2", map2(2, 2));
		eq("_", map2(2, 3));
	}
}