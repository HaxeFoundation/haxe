package cs.system.componentmodel;

/** Provides data for the  event handler. */
@:native("System.ComponentModel.DoWorkEventArgs")
extern class DoWorkEventArgs extends cs.system.componentmodel.CancelEventArgs {
	/**
	 * Gets a value that represents the argument of an asynchronous operation.
	 * @return An  representing the argument of an asynchronous operation.
	 */
	var Argument(default, never):Dynamic;
	/**
	 * Gets or sets a value that represents the result of an asynchronous operation.
	 * @return An  representing the result of an asynchronous operation.
	 */
	var Result(default, default):Dynamic;
	function new(argument:Dynamic):Void;
}
