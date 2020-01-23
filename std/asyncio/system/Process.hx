package asyncio.system;

import haxe.io.Bytes;
import haxe.NoData;
import haxe.errors.NotImplemented;
import haxe.Callback;

/**
	Process execution API
**/
class Process {
	/** Process id */
	public final pid:Int;

	//TODO: this is a dummy constructor to make the compiler shut up about uninitialized finals.
	function new() {
		pid = -1;
	}

	/**
		Execute `command` with `args` command line arguments, wait for the command
		to fully finish and invoke `callback` with the exit code and the contents
		of stdout, and stderr.

		In case the command didn't emit anything to stdout or stdin, the respective
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
	static public function open(command:String, args:Array<String>, options:ProcessOptions, callback:Callback<Null<Process>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Send `signal` to this process.

		This function does not wait for the process to finish.

		@see `asyncio.system.Signal`
	**/
	public function sendSignal(signal:Int, callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}

	/**
		Wait the process to shutdown and get the exit code.
		If the process is already dead at the moment of this call, then `callback`
		may be invoked with the exit code immediately.
	**/
	public function exitCode(callback:Callback<Int>) {
		callback.fail(new NotImplemented());
	}

	/**
		Close the process handle and release associated resources.

		TODO: should this method wait for the process to finish?
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}