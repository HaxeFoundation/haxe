package cases.issues;

import utest.Assert;

class Issue12846 extends TestCase {
	function test(_) {
		vfs.putContent("App.hx", "
			@:build(Macro.build())
			class App {
				static function main() {}
			}
		");
		vfs.putContent("Macro.hx", "
			import haxe.macro.Context;
			class Macro {
				static function build() {
					trace('building_macro');
					Context.registerModuleDependency(Context.getLocalModule(), 'dependency.txt');
					return Context.getBuildFields();
				}
			}
		");
		vfs.putContent("dependency.txt", "initial");

		var args = ["-main", "App.hx", "--no-output", "-js", "no.js", "-v"];

		runHaxe(args);
		assertSuccess();
		Assert.isTrue(hasMessage("building_macro"));

		// wait to ensure timestamp difference
		Sys.sleep(1);

		// change file
		vfs.overwriteContent("dependency.txt", "changed");
		runHaxe(args);
		assertSuccess();
		Assert.isTrue(hasMessage("building_macro"));

		// no change, should reuse App
		runHaxe(args);
		assertSuccess();
		assertReuse("App");
		Assert.isFalse(hasMessage("building_macro"));
	}
}
