package asyncio.system;

import haxe.ds.ReadOnlyArray;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.errors.NotImplemented;
import haxe.Callback;

/**
	Process execution API
**/
class Process {
	/**
		Current process handle.
		Can be used to communicate with the parent process and for self-signalling.
	**/
	static public var current(get,never):CurrentProcess;
	static function get_current():CurrentProcess throw new NotImplemented();

	/**
		Process id.
	**/
	public final pid:Int;

	/**
		Initial IO streams opened for this process.
		The first three indices always are:
		- 0 - stdin
		- 1 - stdout
		- 2 - stderr
		Indices from 3 and higher contain handlers for streams created as configured
		by the corresponding indices in `options.stdio` field of `options` argument
		for `asyncio.system.Process.open` call.
		@see asyncio.system.ProcessOptions.stdio
	**/
	public var stdio(get,never):ReadOnlyArray<Stream>;
	function get_stdio():ReadOnlyArray<Stream> throw new NotImplemented();

	//TODO: this is a dummy constructor to make the compiler shut up about uninitialized finals.
	function new() {
		pid = -1;
	}

	/**
		Execute `command` with `args` command line arguments, wait for the command
		to fully finish and invoke `callback` with the exit code and the contents
		of stdout, and stderr.

		In case the command didn't emit anything to stdout or stderr, the respective
		field of the result structure will be `null`.

		@see asyncio.system.ProcessOptions for various process configuration options.
	 */
	static public function execute(command:String, args:Array<String>, options:ProcessOptions, callback:Callback<Null<{?stdout:Bytes, ?stderr:Bytes, exitCode:Int}>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Start `command` execution with `args` command line arguments.

		@see asyncio.system.ProcessOptions for various process configuration options.
	 */
	static public function open(command:String, args:Array<String>, options:ProcessOptions, callback:Callback<Null<ChildProcess>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Send `signal` to this process.

		This function does not wait for the process to finish.
		The `callback` only indicates if the signal was sent successfully.

		@see asyncio.system.Signal
	**/
	public function sendSignal(signal:Signal, callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}