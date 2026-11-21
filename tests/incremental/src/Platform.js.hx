import hxcoro.Coro.*;
import hxcoro.CoroRun;
import hxcoro.task.NodeLambda;
import js.node.Buffer;
import js.node.ChildProcess;

/**
	What the runner needs beyond the standard library and hxcoro, on node. The
	rest of the runner uses `sys` and coroutines only, so that another target
	only needs this module.
**/
class Platform {
	/** Runs [main] and exits with the code it returns (2 if it throws). **/
	public static function run(main:NodeLambda<Int>) {
		CoroRun.promise(main).then(code -> js.Node.process.exitCode = code, e -> {
			Sys.println('runner error: $e');
			js.Node.process.exitCode = 2;
		});
	}

	public static function cpuCount():Int {
		return js.node.Os.cpus().length;
	}

	/** Whether a program accepts connections on [port] of the local host. **/
	@:coroutine public static function portInUse(port:Int):Bool {
		return suspend(cont -> {
			var s = js.node.Net.connect({host: "127.0.0.1", port: port});
			var done = false;
			function finish(inUse:Bool) {
				if (done)
					return;
				done = true;
				s.destroy();
				cont.resume(inUse, null);
			}
			s.setTimeout(500);
			s.on("connect", () -> finish(true));
			s.on("error", _ -> finish(false));
			s.on("timeout", () -> finish(false));
		});
	}

	/** Sets the modification time of [path] to [time] (seconds, with a fraction). **/
	public static function setModificationTime(path:String, time:Float) {
		var date = Date.fromTime(time * 1000);
		js.node.Fs.utimesSync(path, date, date);
	}
}

/**
	A child process whose output is collected, waited for by coroutines (so
	that the other cases keep running). Cancelling a coroutine waiting in
	`exitCode` kills the process.
**/
class Proc {
	final process:js.node.child_process.ChildProcess;
	final out:Array<Buffer> = [];
	final err:Array<Buffer> = [];
	var code:Null<Int> = null;
	var error:Null<haxe.Exception> = null;
	var onExit:Null<() -> Void> = null;

	/** [env] adds variables to the environment of the runner. **/
	public function new(cmd:String, args:Array<String>, ?env:Map<String, String>) {
		if (env == null) {
			process = ChildProcess.spawn(cmd, args);
		} else {
			var vars = new haxe.DynamicAccess<String>();
			for (k => v in js.Node.process.env)
				vars[k] = v;
			for (k => v in env)
				vars[k] = v;
			process = ChildProcess.spawn(cmd, args, {env: vars});
		}
		process.stdout.on("data", (b:Buffer) -> out.push(b));
		process.stderr.on("data", (b:Buffer) -> err.push(b));
		process.on("error", (e:js.lib.Error) -> {
			error = new haxe.Exception('could not run $cmd: ${e.message}');
			finish(-1);
		});
		// after the output streams are closed
		process.on("close", (code:Null<Int>, _) -> finish(code == null ? -1 : code));
	}

	/** Runs [cmd] to its end. **/
	@:coroutine public static function run(cmd:String, args:Array<String>, ?env:Map<String, String>):Proc {
		var p = new Proc(cmd, args, env);
		p.exitCode();
		return p;
	}

	function finish(code:Int) {
		if (this.code != null)
			return;
		this.code = code;
		if (onExit != null)
			onExit();
	}

	/** What the process wrote to its standard output so far. **/
	public function stdout():String {
		return Buffer.concat(out).toString();
	}

	/** What the process wrote to its standard error so far. **/
	public function stderr():String {
		return Buffer.concat(err).toString();
	}

	public function kill() {
		if (code == null)
			process.kill();
	}

	/** Waits for the process to end and returns its exit code (-1 if it was killed). **/
	@:coroutine public function exitCode():Int {
		if (code == null)
			suspendCancellable(cont -> {
				onExit = () -> cont.resume(null, null);
				_ -> kill();
			});
		if (error != null)
			throw error;
		return code;
	}
}
