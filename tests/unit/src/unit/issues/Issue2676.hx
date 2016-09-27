package unit.issues;
import unit.Test;


class Issue2676 extends Test {
	function test() {
		function match1(s:String) {
			return switch(s) {
				case "foo": 1;
				case _.toUpperCase() => "BAR": 2;
				case _ + "," + _ => "abc,abc": 3;
				case _: 4;
			}
		}

		eq(1, match1("foo"));
		eq(4, match1("foo2"));
		eq(2, match1("bAr"));
		eq(2, match1("BAR"));
		eq(2, match1("bar"));
		eq(4, match1("barz"));
		eq(3, match1("abc"));
		eq(4, match1("ab"));

		// check side effect handling
		function func(i1:Int, i2:Int, i3:Int) {
			return '$i1;$i2;$i3';
		}

		var i = 0;

		var s = switch(9) {
			case func(i++, i++, i++) => "0;1;2": "ok";
			case _: "not ok";
		}

		eq("ok", s);
	}
}