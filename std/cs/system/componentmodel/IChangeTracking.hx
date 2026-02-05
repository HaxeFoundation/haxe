package cs.system.componentmodel;

/** Defines the mechanism for querying the object for changes and resetting of the changed status. */
@:native("System.ComponentModel.IChangeTracking")
extern interface IChangeTracking {
	/**
	 * Gets the object's changed status.
	 * @return if the object's content has changed since the last call to ; otherwise,
	 * .
	 */
	var IsChanged(default, never):Bool;
	/** Resets the object's state to unchanged by accepting the modifications. */
	function AcceptChanges():Void;
}
