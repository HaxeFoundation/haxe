package cs.system.componentmodel;

/** Indicates whether a class converts property change events to  events. */
@:native("System.ComponentModel.IRaiseItemChangedEvents")
extern interface IRaiseItemChangedEvents {
	/**
	 * Gets a value indicating whether the  object raises  events.
	 * @return if the  object raises  events when one of its property values changes;
	 * otherwise, .
	 */
	var RaisesItemChangedEvents(default, never):Bool;
}
