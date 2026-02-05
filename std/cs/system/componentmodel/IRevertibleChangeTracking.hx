package cs.system.componentmodel;

/** Provides support for rolling back the changes */
@:native("System.ComponentModel.IRevertibleChangeTracking")
extern interface IRevertibleChangeTracking extends cs.system.componentmodel.IChangeTracking {
	/** Resets the object's state to unchanged by rejecting the modifications. */
	function RejectChanges():Void;
}
