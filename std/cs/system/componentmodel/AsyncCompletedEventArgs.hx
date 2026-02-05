package cs.system.componentmodel;

/** Provides data for the MethodName event. */
@:native("System.ComponentModel.AsyncCompletedEventArgs")
extern class AsyncCompletedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets a value indicating whether an asynchronous operation has been canceled.
	 * @return if the background operation has been canceled; otherwise . The default
	 * is .
	 */
	var Cancelled(default, never):Bool;
	/**
	 * Gets a value indicating which error occurred during an asynchronous operation.
	 * @return An  instance, if an error occurred during an asynchronous operation;
	 * otherwise .
	 */
	var Error(default, never):cs.system.Exception;
	/**
	 * Gets the unique identifier for the asynchronous task.
	 * @return An object reference that uniquely identifies the asynchronous task;
	 * otherwise,  if no value has been set.
	 */
	var UserState(default, never):Dynamic;
	function new(error:cs.system.Exception, cancelled:Bool, userState:Dynamic):Void;
}
