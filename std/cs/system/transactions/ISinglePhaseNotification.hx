package cs.system.transactions;

/** Describes a resource object that supports single phase commit optimization to participate in a transaction. */
@:native("System.Transactions.ISinglePhaseNotification")
extern interface ISinglePhaseNotification extends cs.system.transactions.IEnlistmentNotification {
	/**
	 * Represents the resource manager's implementation of the callback for the single
	 * phase commit optimization.
	 * @param singlePhaseEnlistment A  used to send a response to the transaction
	 * manager.
	 */
	function SinglePhaseCommit(singlePhaseEnlistment:cs.system.transactions.SinglePhaseEnlistment):Void;
}
