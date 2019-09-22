import haxe.io.Bytes;
import asys.io.*;
import asys.*;
import utest.Assert;
#if hl
import hl.Uv;
#elseif eval
import eval.Uv;
#elseif neko
import neko.Uv;
#end

class TestBase {
	static var helpers:Map<Process, {?exit:ProcessExit}> = [];

	public static function uvSetup():Void {

	}

	public static function uvTeardown():Void {
		helperTeardown();
	}

	/**
		The helper script should be in `test-helpers/<current target>`:

		- `eval` - `test-helpers/eval/<name>.hxml`; will be executed with the hxml
			and `--run <Name>` appended in order to support passing arguments.
		- `hl` - `test-helpers/hl/<name>.hl`
	**/
	public static function helperStart(name:String, ?args:Array<String>, ?options:asys.Process.ProcessSpawnOptions):Process {
		if (args == null)
			args = [];
		var proc:Process;
		#if eval
		args.unshift(name.charAt(0).toUpperCase() + name.substr(1));
		args.unshift("--run");
		args.unshift('test-helpers/eval/$name.hxml');
		name = "haxe";
		#elseif hl
		args.unshift('test-helpers/hl/$name.hl');
		name = "hl";
		#else
		throw "unsupported platform for helperStart";
		#end
		proc = Process.spawn(name, args, options);
		helpers[proc] = {};
		proc.exitSignal.on(exit -> helpers[proc].exit = exit);
		return proc;
	}

	public static function helperTeardown():Void {
		var anyFail = false;
		for (proc => res in helpers) {
			if (res.exit == null) {
				proc.kill();
				proc.close();
				anyFail = true;
			}
		}
		helpers = [];
		if (anyFail)
			Assert.fail("helper script(s) not terminated properly");
	}
}
