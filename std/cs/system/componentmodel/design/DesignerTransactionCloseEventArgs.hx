package cs.system.componentmodel.design;

/** Provides data for the  and  events. */
@:native("System.ComponentModel.Design.DesignerTransactionCloseEventArgs")
extern class DesignerTransactionCloseEventArgs extends cs.system.EventArgs {
	/**
	 * Gets a value indicating whether this is the last transaction to close.
	 * @return , if this is the last transaction to close; otherwise, .
	 */
	var LastTransaction(default, never):Bool;
	/**
	 * Indicates whether the designer called  on the transaction.
	 * @return if the designer called  on the transaction; otherwise, .
	 */
	var TransactionCommitted(default, never):Bool;
	@:overload(function(commit:Bool):Void {})
	function new(commit:Bool, lastTransaction:Bool):Void;
}
