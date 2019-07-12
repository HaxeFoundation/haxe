package unit.issues;

import utest.Assert;

class Issue7376 extends unit.Test {
	function test() {
		foo(bool -> {
			try {
				intJob();
			} catch(e:Dynamic) {
				voidJob();
			}
		});

		foo(bool -> switch bool {
			case true: intJob();
			case false:
		});

		foo(bool -> bool ? intJob() : voidJob());

		noAssert();
	}

	@:pure(false) static function foo(f:Bool->Void) f(true);
	@:pure(false) static function intJob():Int return 0;
	@:pure(false) static function voidJob():Void {}
}