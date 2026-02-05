package cs.system.windows.input;

/** Defines a command. */
@:native("System.Windows.Input.ICommand")
extern interface ICommand {
	/**
	 * Defines the method that determines whether the command can execute in its
	 * current state.
	 * @param parameter Data used by the command.  If the command does not require data
	 * to be passed, this object can be set to .
	 * @return if this command can be executed; otherwise, .
	 */
	function CanExecute(parameter:Dynamic):Bool;
	/**
	 * Defines the method to be called when the command is invoked.
	 * @param parameter Data used by the command.  If the command does not require data
	 * to be passed, this object can be set to .
	 */
	function Execute(parameter:Dynamic):Void;
}
