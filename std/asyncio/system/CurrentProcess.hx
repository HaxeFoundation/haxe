package asyncio.system;

import haxe.errors.NotImplemented;

/**
	Additional API for the current process.

	@see asyncio.system.Process.current
**/
class CurrentProcess extends Process {
	/**
		A stream used by the process as standard input.
	**/
	public var stdin(get,never):IReadable;
	function get_stdin():IReadable throw new NotImplemented();

	/**
		A stream used by the process as standard output.
	**/
	public var stdout(get,never):IWritable;
	function get_stdout():IWritable throw new NotImplemented();

	/**
		A stream used by the process as standard error output.
	**/
	public var stderr(get,never):IWritable;
	function get_stderr():IWritable throw new NotImplemented();
}