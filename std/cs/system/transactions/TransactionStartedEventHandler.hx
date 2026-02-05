package cs.system.transactions;

/**
 * Represents the method that will handle the  event of a  class.
 * @param sender The source of the event.
 * @param e The  that contains the transaction from which transaction information
 * can be retrieved.
 */
@:native("System.Transactions.TransactionStartedEventHandler")
extern class TransactionStartedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.transactions.TransactionEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.transactions.TransactionEventArgs):Void;
}
