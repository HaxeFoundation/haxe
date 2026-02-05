package cs.system.componentmodel;

/** Provides data for the  event. */
@:native("System.ComponentModel.ProgressChangedEventArgs")
extern class ProgressChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the asynchronous task progress percentage.
	 * @return A percentage value indicating the asynchronous task progress.
	 */
	var ProgressPercentage(default, never):Int;
	/**
	 * Gets a unique user state.
	 * @return A unique  indicating the user state.
	 */
	var UserState(default, never):Dynamic;
	function new(progressPercentage:Int, userState:Dynamic):Void;
}
