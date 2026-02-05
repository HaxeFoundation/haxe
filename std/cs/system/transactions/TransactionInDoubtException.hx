package cs.system.transactions;

/** The exception that is thrown when an operation is attempted on a transaction that is in doubt, or an attempt is made to commit the transaction and the transaction becomes InDoubt. */
@:native("System.Transactions.TransactionInDoubtException")
extern class TransactionInDoubtException extends cs.system.transactions.TransactionException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
