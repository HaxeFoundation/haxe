package cs.system.transactions;

/** Describes a delegated transaction for an existing transaction that can be escalated to be managed by the MSDTC when needed. */
@:native("System.Transactions.ITransactionPromoter")
extern interface ITransactionPromoter {
	/**
	 * Notifies an enlisted object that an escalation of the delegated transaction has
	 * been requested.
	 * @return A transmitter/receiver propagation token that marshals a distributed
	 * transaction. For more information, see .
	 */
	function Promote():cs.NativeArray<cs.UInt8>;
}
