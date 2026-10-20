package unit.issues;

import haxe.ds.Option;

class Issue4287 extends unit.Test {
	@:analyzer(no_user_var_fusion)
	function test() {
		eq(None, getNone());
		var local = function(t = None) return t;
		eq(None, local());
	}

	static function getNone<T>(t:Option<T> = None):Option<T> {
		return t;
	}
}