package cs.system.transactions;

/**
 * Provides a mechanism for the hosting environment to supply its own default
 * notion of .
 * @return A  object.
 */
@:native("System.Transactions.HostCurrentTransactionCallback")
extern class HostCurrentTransactionCallback extends cs.system.MulticastDelegate {
	function new(func:()->cs.system.transactions.Transaction):Void;
	function Invoke():cs.system.transactions.Transaction;
}
