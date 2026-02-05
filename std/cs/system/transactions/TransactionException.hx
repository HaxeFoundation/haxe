package cs.system.transactions;

/** The exception that is thrown when you attempt to do work on a transaction that cannot accept new work. */
@:native("System.Transactions.TransactionException")
extern class TransactionException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
