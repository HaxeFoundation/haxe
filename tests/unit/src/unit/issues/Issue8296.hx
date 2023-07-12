package unit.issues;

import haxe.ds.Option;

class Issue8296 extends unit.Test {
	function test() {
		var a = option();
		var b = option();
		var s = switch [a, b] {
			case [Some(parse(_) => Some(_)), _] | [_, Some(_)]:
				'foo';
			case _:
				'bar';
		}
		eq("foo", s);
	}

	static function option() return Some('1');
	static function parse(v:String):Option<Int> return Some(1);
}