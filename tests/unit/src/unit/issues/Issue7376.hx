package unit.issues;

import utest.Assert;

class Issue7376 extends unit.Test {
	function testTryCatch() {
		foo(bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				voidJob();
			}
		});

		var fn = bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				voidJob();
			}
		}
		foo(fn);

		var fn = bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				return;
			}
		}
		foo(fn);

		noAssert();
	}

	function testSwitch() {
		foo(bool -> switch bool {
			case true: intJob();
			case false:
		});

		var fn = bool -> switch bool {
			case true: intJob();
			case false:
		}
		foo(fn);

		var fn = bool -> switch bool {
			case true: intJob();
			case false: return;
		}
		foo(fn);

		noAssert();
	}

	function testIfElse() {
		foo(bool -> bool ? intJob() : voidJob());

		var fn = bool -> if(bool) intJob() else {};
		foo(fn);

		var fn = bool -> if (bool) intJob() else return;
		foo(fn);

		noAssert();
	}

	@:pure(false) static function foo(f:(Bool)->Void) f(true);
	@:pure(false) static function intJob():Int return 0;
	@:pure(false) static function voidJob():Void {}
}