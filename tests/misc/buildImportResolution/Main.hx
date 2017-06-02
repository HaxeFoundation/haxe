import TestMacro.*;
import TestMacro2.build;
import TestMacro2.build as buildFoo;
import foo.*;


#if !macro
@:build(build(1, true))
@:build(TestMacro.build(2, true))
@:build(TestMacro.TestMacro.build(3, true))
@:build(foo.TestMacro.build(4, true))
@:build(TestMacro3.build(5, true))
@:build(buildFoo(6, true))
@:build(foo.bar.TestMacro.build(7, true))
#end
class Main {

	macro static function run () {
		build(1, false);
		TestMacro.build(2, false);
		TestMacro.TestMacro.build(3, false);
		foo.TestMacro.build(4, false);
		TestMacro3.build(5, false);
		buildFoo(6, false);
		foo.bar.TestMacro.build(7, false);
		return macro null;
	}

	public static function main () {
		run();
		if (!Stats.check()) {
			Sys.exit(1);
		} else {
			Sys.exit(0);
		}
	}



}