package unit.issues;

import haxe.ds.Option;

private abstract OptionArray(Option<Array<Int>>) from Option<Array<Int>> to Option<Array<Int>> {}

class Issue13071 extends Test {
	function testNestedDynamic() {
		function f(d:Dynamic) {
			return switch d {
				case [[a, b], c]: 'nested $a $b $c';
				case [[]]: "empty";
				case _: "other";
			}
		}
		eq("nested 1 2 3", f([[1, 2], 3]));
		eq("empty", f([[]]));
		eq("other", f([[1]]));
		eq("other", f([]));
	}

	function testNestedArrayDynamic() {
		function f(a:Array<Dynamic>) {
			return switch a {
				case [[[x]]]: 'deep $x';
				case [[x, y]]: 'two $x $y';
				case _: "other";
			}
		}
		eq("deep 5", f([[[5]]]));
		eq("two 1 2", f([[1, 2]]));
		eq("other", f([]));
	}

	function testAbstractOverEnum() {
		function f(a:OptionArray) {
			return switch a {
				case Some([x, y]): 'some $x $y';
				case _: "other";
			}
		}
		eq("some 1 2", f(Some([1, 2])));
		eq("other", f(Some([1])));
		eq("other", f(None));
	}
}
