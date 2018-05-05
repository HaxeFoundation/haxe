package unit.issues;

class Issue4969 extends Test {

	@:keep
	static function test1() {
		var __return = function(x:Dynamic) { return 1; }
		__return(throw null); // Cannot use this expression as value
	}

	@:keep
	static function test2() {
		var __return = function(x:Dynamic) { return 1; }
		__return(return null); // Cannot use this expression as value
		return null;
	}

	@:keep
	static function test3() {
		var __return = function(x:Dynamic) { return 1; }
		while (true) {
			__return(break); // Cannot use this expression as value
		}
	}

	@:keep
	static function test4() {
		var __return = function(x:Dynamic) { return 1; }
		while (true) {
			__return(continue); // Cannot use this expression as value
		}
	}
}