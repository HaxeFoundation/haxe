package cs.system.componentmodel;

/** Provides functionality to commit or rollback changes to an object that is used as a data source. */
@:native("System.ComponentModel.IEditableObject")
extern interface IEditableObject {
	/** Begins an edit on an object. */
	function BeginEdit():Void;
	/** Discards changes since the last  call. */
	function CancelEdit():Void;
	/** Pushes changes since the last  or  call into the underlying object. */
	function EndEdit():Void;
}
