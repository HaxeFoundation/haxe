package cs.system.componentmodel.design;

/** Provides a way to group a series of design-time actions to improve performance and enable most types of changes to be undone. */
@:native("System.ComponentModel.Design.DesignerTransaction")
extern class DesignerTransaction {
	/**
	 * Gets a value indicating whether the transaction was canceled.
	 * @return if the transaction was canceled; otherwise, .
	 */
	var Canceled(default, never):Bool;
	/**
	 * Gets a value indicating whether the transaction was committed.
	 * @return if the transaction was committed; otherwise, .
	 */
	var Committed(default, never):Bool;
	/**
	 * Gets a description for the transaction.
	 * @return A description for the transaction.
	 */
	var Description(default, never):String;
	/** Cancels the transaction and attempts to roll back the changes made by the events of the transaction. */
	function Cancel():Void;
	/** Commits this transaction. */
	function Commit():Void;
}
