package cs.system.transactions;

/** The exception that is thrown when a promotion fails. */
@:native("System.Transactions.TransactionPromotionException")
extern class TransactionPromotionException extends cs.system.transactions.TransactionException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
