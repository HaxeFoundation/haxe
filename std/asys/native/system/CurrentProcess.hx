package asys.native.system;

import haxe.exceptions.NotImplementedException;

/**
	Additional API for the current process.

	@see asys.native.system.Process.current
**/
class CurrentProcess extends Process {
	/**
		A stream used by the process as standard input.
	**/
	public var stdin(get,never):IReadable;
	function get_stdin():IReadable throw new NotImplementedException();

	/**
		A stream used by the process as standard output.
	**/
	public var stdout(get,never):IWritable;
	function get_stdout():IWritable throw new NotImplementedException();

	/**
		A stream used by the process as standard error output.
	**/
	public var stderr(get,never):IWritable;
	function get_stderr():IWritable throw new NotImplementedException();

	/**
		Set the action taken by the process on receipt of a `signal`.

		Possible `action` values:
		- `Ignore` - ignore the signal;
		- `Default` - restore default action;
		- `Handle(handler:() -> Void)` - execute `handler` on `signal` receipt.

		Actions for `Kill` and `Stop` signals cannot be changed.
	**/
	public function setSignalAction(signal:Signal, action:SignalAction):Void {
		throw new NotImplementedException();
	}
}