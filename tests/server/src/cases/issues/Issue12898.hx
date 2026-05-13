package cases.issues;

import haxe.display.FsPath;
import haxe.display.Server;

// Exercises shared top-level JVM closure classes across multiple modules
// under the compilation server. Each test sets up two callers that both
// reference the same target method from different codegen paths; the
// resulting closure class must be registered once and emitted at finalize
// time. The server must keep that class in the jar after invalidating any
// subset of the participating modules and rebuilding, regardless of which
// caller's codegen actually triggered the registration on a given build.
class Issue12898 extends TestCase {
	// Multi-overload merge path: `@:overload @:native('foo')` makes several
	// Haxe fields share a JVM method name; generate_dynamic_access groups
	// them and emits a single shared closure with one invoke per overload.
	function testOverloadedClosureViaReflect(_) {
		vfs.putContent("Target.hx", "
			class Target {
				public function new() {}
				public function foo(x:Dynamic):Void Sys.println('dyn:' + x);
				@:overload @:native('foo') public function fooBool(b:Bool):Void Sys.println('bool:' + b);
			}
		");
		vfs.putContent("CallerA.hx", "
			class CallerA {
				public static function run(t:Target):Void {
					final f = Reflect.field(t, 'foo');
					Reflect.callMethod(t, f, [true]);
				}
			}
		");
		vfs.putContent("CallerB.hx", "
			class CallerB {
				public static function run(t:Target):Void {
					final f = Reflect.field(t, 'foo');
					Reflect.callMethod(t, f, ['hello']);
				}
			}
		");
		vfs.putContent("Main.hx", "
			class Main {
				static function main() {
					final t = new Target();
					CallerA.run(t);
					CallerB.run(t);
					t.fooBool(true);
				}
			}
		");

		runScenario(["-D", "jvm.dynamic-level=2"], "dyn:true\ndyn:hello\nbool:true\n", "Target_foo");
	}

	// Direct member-closure path (`obj.method` as a value): exercises
	// read_anon_field's FClosure branch, which is not gated on
	// dynamic-level and was previously a separate per-caller inner class
	// per (Target, method) — now a single shared top-level class.
	function testDirectMemberClosure(_) {
		vfs.putContent("Target.hx", "
			class Target {
				public function new() {}
				public function greet(name:String):Void Sys.println('hi ' + name);
			}
		");
		vfs.putContent("CallerA.hx", "
			class CallerA {
				public static function run(t:Target):String->Void {
					final f:String->Void = t.greet;
					return f;
				}
			}
		");
		vfs.putContent("CallerB.hx", "
			class CallerB {
				public static function run(t:Target):String->Void {
					final f:String->Void = t.greet;
					return f;
				}
			}
		");
		vfs.putContent("Main.hx", "
			class Main {
				static function main() {
					final t = new Target();
					CallerA.run(t)('A');
					CallerB.run(t)('B');
				}
			}
		");

		runScenario([], "hi A\nhi B\n", "Target_greet");
	}

	// Single-method dynamic-access path: a non-overloaded method reached
	// via Reflect.field at dynamic-level=2 goes through the single-entry
	// generate_dynamic_access branch (different from the overload-merge
	// branch covered above). Two callers must still share one closure.
	function testSingleMethodViaReflect(_) {
		vfs.putContent("Target.hx", "
			class Target {
				public function new() {}
				public function bar(x:Int):Void Sys.println('bar:' + x);
			}
		");
		vfs.putContent("CallerA.hx", "
			class CallerA {
				public static function run(t:Target):Void {
					final f = Reflect.field(t, 'bar');
					Reflect.callMethod(t, f, [1]);
				}
			}
		");
		vfs.putContent("CallerB.hx", "
			class CallerB {
				public static function run(t:Target):Void {
					final f = Reflect.field(t, 'bar');
					Reflect.callMethod(t, f, [2]);
				}
			}
		");
		vfs.putContent("Main.hx", "
			class Main {
				static function main() {
					final t = new Target();
					CallerA.run(t);
					CallerB.run(t);
				}
			}
		");

		runScenario(["-D", "jvm.dynamic-level=2"], "bar:1\nbar:2\n", "Target_bar");
	}

	@:coroutine function runScenario(extraArgs:Array<String>, expectedStdout:String, classNameNeedle:String) {
		final args = ["-main", "Main", "-jvm", "bin/test.jar"].concat(extraArgs);

		@:coroutine function buildAndRun(?label:String) {
			runHaxe(args);
			assertSuccess();
			final r = runJar();
			Assert.equals(0, r.exit, 'jar exited ${r.exit} (${label ?? "?"}): ${r.stderr}');
			Assert.equals(expectedStdout, r.stdout, 'wrong stdout (${label ?? "?"})');
			assertClosureInJar(classNameNeedle, label);
		}

		buildAndRun("clean");

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CallerA.hx")});
		buildAndRun("invalidate CallerA");
		assertReuse("CallerB");

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CallerB.hx")});
		buildAndRun("invalidate CallerB");
		assertReuse("CallerA");

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Target.hx")});
		buildAndRun("invalidate Target");

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CallerA.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CallerB.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Target.hx")});
		buildAndRun("invalidate all");
	}

	function runJar():{exit:Int, stdout:String, stderr:String} {
		final r = js.node.ChildProcess.spawnSync("java", ["-jar", haxe.io.Path.join([testDir, "bin/test.jar"])]);
		return {
			exit: r.status,
			stdout: (r.stdout : js.node.Buffer).toString(),
			stderr: (r.stderr : js.node.Buffer).toString(),
		};
	}

	// Verifies the closure for the target method is materialized exactly
	// once, as a single shared top-level class under `jvm/$Closure/` —
	// proving that the two callers' codegen paths converged on one class
	// instead of producing one inner class per caller.
	function assertClosureInJar(classNameNeedle:String, ?label:String) {
		final bytes = sys.io.File.getBytes(haxe.io.Path.join([testDir, "bin/test.jar"]));
		final entries = haxe.zip.Reader.readZip(new haxe.io.BytesInput(bytes));
		final matches = [
			for (e in entries)
				if (StringTools.endsWith(e.fileName, ".class") && e.fileName.indexOf(classNameNeedle) >= 0)
					e.fileName
		];
		Assert.equals(1, matches.length, 'expected exactly one $classNameNeedle closure class (${label ?? "?"}), got: ${matches.join(", ")}');
		Assert.isTrue(StringTools.startsWith(matches[0], "jvm/$Closure/"),
			'closure class is not top-level under jvm/$$Closure/ (${label ?? "?"}): ${matches[0]}');
	}
}
