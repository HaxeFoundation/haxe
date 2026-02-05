package cs.system.transactions;

/** Provides data for the following transaction events: , . */
@:native("System.Transactions.TransactionEventArgs")
extern class TransactionEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the transaction for which event status is provided.
	 * @return A  for which event status is provided.
	 */
	var Transaction(default, never):cs.system.transactions.Transaction;
	function new():Void;
}
