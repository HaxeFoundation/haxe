package cs.system.transactions;

/** Describes an object that acts as a commit delegate for a non-distributed transaction internal to a resource manager. */
@:native("System.Transactions.IPromotableSinglePhaseNotification")
extern interface IPromotableSinglePhaseNotification extends cs.system.transactions.ITransactionPromoter {
	/** Notifies a transaction participant that enlistment has completed successfully. */
	function Initialize():Void;
	/**
	 * Notifies an enlisted object that the transaction is being rolled back.
	 * @param singlePhaseEnlistment A  object used to send a response to the
	 * transaction manager.
	 */
	function Rollback(singlePhaseEnlistment:cs.system.transactions.SinglePhaseEnlistment):Void;
	/**
	 * Notifies an enlisted object that the transaction is being committed.
	 * @param singlePhaseEnlistment A  interface used to send a response to the
	 * transaction manager.
	 */
	function SinglePhaseCommit(singlePhaseEnlistment:cs.system.transactions.SinglePhaseEnlistment):Void;
}
