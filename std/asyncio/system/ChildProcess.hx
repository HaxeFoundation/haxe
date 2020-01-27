package asyncio.system;

import haxe.ds.ReadOnlyArray;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.errors.NotImplemented;
import haxe.Callback;

/**
	Additional API for child processes spawned by the current process.

	@see asyncio.system.Process.open
**/
class ChildProcess extends Process {
	/**
		A stream used by the process as standard input.
	**/
	public var stdin(get,never):IWritable;
	function get_stdin():IWritable throw new NotImplemented();

	/**
		A stream used by the process as standard output.
	**/
	public var stdout(get,never):IReadable;
	function get_stdout():IReadable throw new NotImplemented();

	/**
		A stream used by the process as standard error output.
	**/
	public var stderr(get,never):IReadable;
	function get_stderr():IReadable throw new NotImplemented();

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