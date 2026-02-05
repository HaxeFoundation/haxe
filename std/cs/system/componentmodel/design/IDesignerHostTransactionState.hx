package cs.system.componentmodel.design;

/** Specifies methods for the designer host to report on the state of transactions. */
@:native("System.ComponentModel.Design.IDesignerHostTransactionState")
extern interface IDesignerHostTransactionState {
	/**
	 * Gets a value indicating whether the designer host is closing a transaction.
	 * @return if the designer is closing a transaction; otherwise, .
	 */
	var IsClosingTransaction(default, never):Bool;
}
