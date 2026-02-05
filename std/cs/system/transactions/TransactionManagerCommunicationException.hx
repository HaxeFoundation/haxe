package cs.system.transactions;

/** The exception that is thrown when a resource manager cannot communicate with the transaction manager. */
@:native("System.Transactions.TransactionManagerCommunicationException")
extern class TransactionManagerCommunicationException extends cs.system.transactions.TransactionException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
