package cs.system.componentmodel;

/** Provides data for events that can be handled completely in an event handler. */
@:native("System.ComponentModel.HandledEventArgs")
extern class HandledEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets a value that indicates whether the event handler has completely
	 * handled the event or whether the system should continue its own processing.
	 * @return if the event has been completely handled; otherwise, .
	 */
	var Handled(default, default):Bool;
	@:overload(function():Void {})
	function new(defaultHandledValue:Bool):Void;
}
