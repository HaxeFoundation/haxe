package asys;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;
import haxe.io.*;
import asys.net.Socket;
import asys.io.*;
import asys.uv.UVProcessSpawnFlags;

private typedef Native =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Process;
	#elseif hl
	hl.uv.Process;
	#elseif neko
	neko.uv.Process;
	#else
	#error "process not supported on this platform"
	#end

private typedef NativeProcessIO =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Process.ProcessIO;
	#elseif hl
	hl.uv.Process.ProcessIO;
	#elseif neko
	neko.uv.Process.ProcessIO;
	#else
	#error "process not supported on this platform"
	#end

/**
	Options for spawning a process. See `Process.spawn`.
**/
typedef ProcessSpawnOptions = {
	?cwd:FilePath,
	?env:Map<String, String>,
	?argv0:String,
	?stdio:Array<ProcessIO>,
	?detached:Bool,
	?uid:Int,
	?gid:Int,
	// ?shell:?,
	?windowsVerbatimArguments:Bool,
	?windowsHide:Bool
};

/**
	Class representing a spawned process.
**/
class Process {
	/**
		Execute the given `command` with `args` (none by default). `options` can be
		specified to change the way the process is spawned.

		`options.stdio` is an optional array of `ProcessIO` specifications which
		can be used to define the file descriptors for the new process:

		- `Ignore` - skip the current position. No stream or pipe will be open for
			this index.
		- `Inherit` - inherit the corresponding file descriptor from the current
			process. Shares standard input, standard output, and standard error in
			index 0, 1, and 2, respectively. In index 3 or higher, `Inherit` has the
			same effect as `Ignore`.
		- `Pipe(readable, writable, ?pipe)` - create or use a pipe. `readable` and
			`writable` specify whether the pipe will be readable and writable from
			the point of view of the spawned process. If `pipe` is given, it is used
			directly, otherwise a new pipe is created.
		- `Ipc` - create an IPC (inter-process comunication) pipe. Only one may be
			specified in `options.stdio`. This special pipe will not have an entry in
			the `stdio` array of the resulting process; instead, messages can be sent
			using the `send` method, and received over `messageSignal`. IPC pipes
			allow sending and receiving structured Haxe data, as well as connected
			sockets and pipes.

		Pipes are made available in the `stdio` array afther the process is
		spawned. Standard file descriptors have their own variables:

		- `stdin` - set to point to a pipe in index 0, if it exists and is
			read-only for the spawned process.
		- `stdout` - set to point to a pipe in index 1, if it exists and is
			write-only for the spawned process.
		- `stderr` - set to point to a pipe in index 2, if it exists and is
			write-only for the spawned process.

		If `options.stdio` is not given,
		`[Pipe(true, false), Pipe(false, true), Pipe(false, true)]` is used as a
		default.

		@param options.cwd Path to the working directory. Defaults to the current
			working directory if not given.
		@param options.env Environment variables. Defaults to the environment
			variables of the current process if not given.
		@param options.argv0 First entry in the `argv` array for the spawned
			process. Defaults to `command` if not given.
		@param options.stdio Array of `ProcessIO` specifications, see above.
		@param options.detached When `true`, creates a detached process which can
			continue running after the current process exits. Note that `unref` must
			be called on the spawned process otherwise the event loop of the current
			process is kept allive.
		@param options.uid User identifier.
		@param options.gid Group identifier.
		@param options.windowsVerbatimArguments (Windows only.) Do not perform
			automatic quoting or escaping of arguments.
		@param options.windowsHide (Windows only.) Automatically hide the window of
			the spawned process.
	**/
	public static function spawn(command:String, ?args:Array<String>, ?options:ProcessSpawnOptions):Process {
		var proc = new Process();
		var flags:UVProcessSpawnFlags = None;
		if (options == null)
			options = {};
		if (options.detached)
			flags |= UVProcessSpawnFlags.Detached;
		if (options.uid != null)
			flags |= UVProcessSpawnFlags.SetUid;
		if (options.gid != null)
			flags |= UVProcessSpawnFlags.SetGid;
		if (options.windowsVerbatimArguments)
			flags |= UVProcessSpawnFlags.WindowsVerbatimArguments;
		if (options.windowsHide)
			flags |= UVProcessSpawnFlags.WindowsHide;
		if (options.stdio == null)
			options.stdio = [Pipe(true, false), Pipe(false, true), Pipe(false, true)];
		var stdin:IWritable = null;
		var stdout:IReadable = null;
		var stderr:IReadable = null;
		var stdioPipes = [];
		var ipc:Socket = null;
		var nativeStdio:Array<NativeProcessIO> = [
			for (i in 0...options.stdio.length)
				switch (options.stdio[i]) {
					case Ignore:
						Ignore;
					case Inherit:
						Inherit;
					case Pipe(r, w, pipe):
						if (pipe == null) {
							pipe = Socket.create();
							@:privateAccess pipe.initPipe(false);
						} else {
							if (@:privateAccess pipe.native == null)
								throw "invalid pipe";
						}
						switch (i) {
							case 0 if (r && !w):
								stdin = pipe;
							case 1 if (!r && w):
								stdout = pipe;
							case 2 if (!r && w):
								stderr = pipe;
							case _:
						}
						stdioPipes[i] = pipe;
						Pipe(r, w, @:privateAccess pipe.native);
					case Ipc:
						if (ipc != null)
							throw "only one IPC pipe can be specified for a process";
						ipc = Socket.create();
						@:privateAccess ipc.initPipe(true);
						Ipc(@:privateAccess ipc.native);
				}
		];
		var args = args != null ? args : [];
		if (options.argv0 != null)
			args.unshift(options.argv0);
		else
			args.unshift(command);
		var native = new Native(
			(err, data) -> proc.exitSignal.emit(data),
			command,
			args,
			options.env != null ? [ for (k => v in options.env) '$k=$v' ] : [],
			options.cwd != null ? @:privateAccess options.cwd.get_raw() : Sys.getCwd(),
			flags,
			nativeStdio,
			options.uid != null ? options.uid : 0,
			options.gid != null ? options.gid : 0
		);
		proc.native = native;
		if (ipc != null) {
			proc.connected = true;
			proc.ipc = ipc;
			proc.ipcOut = @:privateAccess new asys.io.IpcSerializer(ipc);
			proc.ipcIn = @:privateAccess new asys.io.IpcUnserializer(ipc);
			proc.messageSignal = new ArraySignal(); //proc.ipcIn.messageSignal;
			proc.ipcIn.messageSignal.on(message -> proc.messageSignal.emit(message));
		}
		proc.stdin = stdin;
		proc.stdout = stdout;
		proc.stderr = stderr;
		proc.stdio = stdioPipes;
		return proc;
	}

	/**
		Emitted when `this` process and all of its pipes are closed.
	**/
	public final closeSignal:Signal<NoData> = new ArraySignal();

	// public final disconnectSignal:Signal<NoData> = new ArraySignal(); // IPC

	/**
		Emitted when an error occurs during communication with `this` process.
	**/
	public final errorSignal:Signal<Error> = new ArraySignal();

	/**
		Emitted when `this` process exits, potentially due to a signal.
	**/
	public final exitSignal:Signal<ProcessExit> = new ArraySignal();

	/**
		Emitted when a message is received over IPC. The process must be created
		with an `Ipc` entry in `options.stdio`; see `Process.spawn`.
	**/
	public var messageSignal(default, null):Signal<IpcMessage>;

	public var connected(default, null):Bool = false;
	public var killed:Bool;

	private function get_pid():Int {
		return native.getPid();
	}

	/**
		Process identifier of `this` process. A PID uniquely identifies a process
		on its host machine for the duration of its lifetime.
	**/
	public var pid(get, never):Int;

	/**
		Standard input. May be `null` - see `options.stdio` in `spawn`.
	**/
	public var stdin:IWritable;

	/**
		Standard output. May be `null` - see `options.stdio` in `spawn`.
	**/
	public var stdout:IReadable;

	/**
		Standard error. May be `null` - see `options.stdio` in `spawn`.
	**/
	public var stderr:IReadable;

	/**
		Pipes created between the current (host) process and `this` (spawned)
		process. The order corresponds to the `ProcessIO` specifiers in
		`options.stdio` in `spawn`. This array can be used to access non-standard
		pipes, i.e. file descriptors 3 and higher, as well as file descriptors 0-2
		with non-standard read/write access.
	**/
	public var stdio:Array<Socket>;

	var native:Native;
	var ipc:Socket;
	var ipcOut:asys.io.IpcSerializer;
	var ipcIn:asys.io.IpcUnserializer;

	// public function disconnect():Void; // IPC

	/**
		Send a signal to `this` process.
	**/
	public function kill(?signal:Int = 7):Void {
		native.kill(signal);
	}

	/**
		Close `this` process handle and all pipes in `stdio`.
	**/
	public function close(?cb:Callback<NoData>):Void {
		var needed = 1;
		var closed = 0;
		function close(err:Error, _:NoData):Void {
			closed++;
			if (closed == needed && cb != null)
				cb(null, new NoData());
		}
		for (pipe in stdio) {
			if (pipe != null) {
				needed++;
				pipe.destroy(close);
			}
		}
		if (connected) {
			needed++;
			ipc.destroy(close);
		}
		native.close(close);
	}

	/**
		Send `data` to the process over the IPC channel. The process must be
		created with an `Ipc` entry in `options.stdio`; see `Process.spawn`.
	**/
	public function send(message:IpcMessage):Void {
		if (!connected)
			throw "IPC not connected";
		ipcOut.write(message);
	}

	public function ref():Void {
		native.ref();
	}

	public function unref():Void {
		native.unref();
	}

	private function new() {}
}
