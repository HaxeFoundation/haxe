package cases.issues;

class Issue12443 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Test.{-1-}
			}
		}
	**/
	function test(_) {
		vfs.putContent("Test.hx", "
			class Test {
				public static function foo():Void {}
				public static function name() {}
			}
		");
		vfs.putContent("Main.hx", "
			class Main {
				static function main() {
					Test.foo;
				}
			}
		");
		var args = ["-main", "Main", "--interp"];
		runHaxeJson(args, ServerMethods.Configure, {noModuleChecks: true});
		runHaxe(args);
		assertSuccess();

		vfs.putContent("Test.hx", "
			class Test {
				public static function foo2():Void {}
				public static function name() {}
			}
		");
		var testFullPath = new FsPath(sys.FileSystem.fullPath(testDir + "/Test.hx"));
		runHaxeJson(args, ServerMethods.Invalidate, {file: testFullPath});

		vfs.putContent("Main.hx", source);
		var mainFullPath = new FsPath(sys.FileSystem.fullPath(testDir + "/Main.hx"));
		runHaxeJson(args, ServerMethods.Invalidate, {file: mainFullPath});

		var res = runHaxeJson(args, DisplayMethods.Completion, {file: mainFullPath, offset: offset(1), wasAutoTriggered: false});
		var names = [for (item in res.items) item.args.field.name];
		Assert.contains("foo2", names);
		Assert.notContains("foo", names);
	}
}
