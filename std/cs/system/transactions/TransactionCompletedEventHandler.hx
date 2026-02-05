package cs.system.transactions;

/**
 * Represents the method that handles the  event of a  class.
 * @param sender The source of the event.
 * @param e The  that contains the event data.
 */
@:native("System.Transactions.TransactionCompletedEventHandler")
extern class TransactionCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.transactions.TransactionEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.transactions.TransactionEventArgs):Void;
}
