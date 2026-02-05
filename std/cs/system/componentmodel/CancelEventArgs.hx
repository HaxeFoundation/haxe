package cs.system.componentmodel;

/** Provides data for a cancelable event. */
@:native("System.ComponentModel.CancelEventArgs")
extern class CancelEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets a value indicating whether the event should be canceled.
	 * @return if the event should be canceled; otherwise, .
	 */
	var Cancel(default, default):Bool;
	@:overload(function():Void {})
	function new(cancel:Bool):Void;
}
