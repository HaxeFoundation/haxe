package cs.system.transactions;

/** The exception that is thrown when an operation is attempted on a transaction that has already been rolled back, or an attempt is made to commit the transaction and the transaction aborts. */
@:native("System.Transactions.TransactionAbortedException")
extern class TransactionAbortedException extends cs.system.transactions.TransactionException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
