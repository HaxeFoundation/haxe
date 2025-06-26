package eval.luv;

import eval.integers.Int64;

/**
	File descriptor redirections for use with `eval.luv.Process.spawn`
**/
@:coreType abstract Redirection {}

/**
	Options for spawning the process.
**/
typedef ProcessOptions = {
	var ?onExit:(p:Process, exitStatus:Int64, termSignal:Int)->Void;
	var ?environment:Map<String,NativeString>;
	var ?workingDirectory:NativeString;
	var ?redirect:Array<Redirection>;
	var ?uid:Int;
	var ?gid:Int;
	var ?windowsVerbatimArguments:Bool;
	var ?detached:Bool;
	var ?windowsHide:Bool;
	var ?windowsHideConsole:Bool;
	var ?windowsHideGui:Bool;
}

/**
	Subprocesses.

	@see https://aantron.github.io/luv/luv/Luv/Process
**/
@:using(eval.luv.Handle)
@:coreType abstract Process to Handle {
	extern static public final stdin:Int;
	extern static public final stdout:Int;
	extern static public final stderr:Int;

	/**
		Causes `fd` in the child to be connected to `toParentPipe` in the parent.

		Binds `UV_CREATE_PIPE`.

		`readableInChild` sets `UV_READABLE_PIPE`, and `writableInChild` sets `UV_WRITABLE_PIPE`.

		`overlapped` sets `UV_OVERLAPPED_PIPE`.
	**/
	static public function toParentPipe(fd:Int, parentPipe:Pipe, readableInChild:Bool, writableInChild:Bool, overlapped:Bool):Redirection;

	/**
		Causes `fd` in the child to be connected to the same device or peer as `fromParentFd` in the parent.

		Binds `UV_INHERIT_FD`
	**/
	static public function inheritFd(fd:Int, fromParentFd:Int):Redirection;

	/**
		Same as `eval.luv.Process.inheritFd`, but takes an `eval.luv.Stream` for the parent file descriptor.

		Binds `UV_INHERIT_STREAM`.
	**/
	static public function inheritStream(fd:Int, fromParentStream:Stream):Redirection;

	/**
		Starts a process.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function spawn(loop:Loop, cmd:NativeString, args:Array<NativeString>, ?options:ProcessOptions):Result<Process>;

	/**
		Disables (tries) file descriptor inheritance for inherited descriptors.
	**/
	static public function disableStdioInheritance():Void;

	/**
		Sends the given signal to the process with the given pid.
	**/
	static public function killPid(pid:Int, sigNum:Signal.SigNum):Result<Result.NoData>;

	/**
		Sends the given signal to the process.
	**/
	public function kill(sigNum:Signal.SigNum):Result<Result.NoData>;

	/**
		Evaluates to the pid of the process.
	**/
	public function pid():Int;
}