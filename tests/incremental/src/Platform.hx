import hxcoro.Coro.*;
import hxcoro.CoroRun;
import hxcoro.task.NodeLambda;
import sys.thread.Lock;
import sys.thread.Mutex;
import sys.thread.Thread;

/**
	What the runner needs beyond the standard library and hxcoro, on threaded
	sys targets (`Platform.js.hx` is the node version). The coroutines run on
	hxcoro's thread pool, so the cases run on several threads.
**/
class Platform {
	/** Runs [main] and exits with the code it returns (2 if it throws). **/
	public static function run(main:NodeLambda<Int>) {
		var code = try CoroRun.run(main) catch (e:haxe.Exception) {
			Sys.println('runner error: ${e.details()}');
			2;
		}
		Sys.exit(code);
	}

	public static function cpuCount():Int {
		#if jvm
		return java.lang.Runtime.getRuntime().availableProcessors();
		#else
		return 4;
		#end
	}

	/** Whether a program accepts connections on [port] of the local host. **/
	public static function portInUse(port:Int):Bool {
		var s = new sys.net.Socket();
		try {
			s.setTimeout(0.5);
			s.connect(new sys.net.Host("127.0.0.1"), port);
			s.close();
			return true;
		} catch (_:Dynamic) {
			s.close();
			return false;
		}
	}

	/** Sets the modification time of [path] to [time] (seconds, with a fraction). **/
	public static function setModificationTime(path:String, time:Float) {
		#if jvm
		java.nio.file.Files.setLastModifiedTime(java.nio.file.Paths.get(path, ...[]),
			java.nio.file.attribute.FileTime.fromMillis(haxe.Int64.fromFloat(time * 1000)));
		#else
		throw new haxe.exceptions.NotImplementedException();
		#end
	}
}

/**
	A child process whose output is collected, waited for by coroutines (a
	thread waits for the process and resumes them). Cancelling a coroutine
	waiting in `exitCode` kills the process.
**/
class Proc {
	final process:sys.io.Process;
	final mutex = new Mutex();
	var out = "";
	var err = "";
	var code:Null<Int> = null;
	var onExit:Null<() -> Void> = null;

	public function new(cmd:String, args:Array<String>) {
		process = new sys.io.Process(cmd, args);
		Thread.create(() -> {
			// both pipes are read at the same time, so that a process filling one cannot block
			var errRead = new Lock();
			var err = "";
			Thread.create(() -> {
				err = readAll(process.stderr);
				errRead.release();
			});
			var out = readAll(process.stdout);
			errRead.wait();
			// the pipes of a killed process are closed, and the blocking `exitCode` fails reading them
			var code:Null<Int> = try process.exitCode() catch (_:Dynamic) null;
			while (code == null) {
				Sys.sleep(0.01);
				code = process.exitCode(false);
			}
			mutex.acquire();
			process.close();
			this.out = out;
			this.err = err;
			this.code = code;
			var callback = onExit;
			mutex.release();
			if (callback != null)
				callback();
		});
	}

	static function readAll(input:haxe.io.Input):String {
		return try input.readAll().toString() catch (_:Dynamic) "";
	}

	/** Runs [cmd] to its end. **/
	@:coroutine public static function run(cmd:String, args:Array<String>):Proc {
		var p = new Proc(cmd, args);
		p.exitCode();
		return p;
	}

	/** What the process wrote to its standard output (once it ended). **/
	public function stdout():String {
		return out;
	}

	/** What the process wrote to its standard error (once it ended). **/
	public function stderr():String {
		return err;
	}

	public function kill() {
		mutex.acquire();
		if (code == null)
			process.kill();
		mutex.release();
	}

	/** Waits for the process to end and returns its exit code. **/
	@:coroutine public function exitCode():Int {
		suspendCancellable(cont -> {
			mutex.acquire();
			var exited = code != null;
			if (!exited)
				onExit = () -> cont.resume(null, null);
			mutex.release();
			if (exited)
				cont.resume(null, null);
			_ -> kill();
		});
		return code;
	}
}
