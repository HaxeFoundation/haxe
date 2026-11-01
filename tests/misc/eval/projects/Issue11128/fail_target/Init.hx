package fail_target;

import haxe.macro.Compiler;
import haxe.macro.PlatformConfig;

class Init {
	public static function init() {
		// Do something to fail to check that errors work here.
		final a = 123;
		final b: String = a;
	}
}
