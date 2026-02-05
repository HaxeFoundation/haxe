package cs.system.transactions;

/** Describes an interface that a resource manager should implement to provide two phase commit notification callbacks for the transaction manager upon enlisting for participation. */
@:native("System.Transactions.IEnlistmentNotification")
extern interface IEnlistmentNotification {
	/**
	 * Notifies an enlisted object that a transaction is being committed.
	 * @param enlistment An  object used to send a response to the transaction manager.
	 */
	function Commit(enlistment:cs.system.transactions.Enlistment):Void;
	/**
	 * Notifies an enlisted object that the status of a transaction is in doubt.
	 * @param enlistment An  object used to send a response to the transaction manager.
	 */
	function InDoubt(enlistment:cs.system.transactions.Enlistment):Void;
	/**
	 * Notifies an enlisted object that a transaction is being prepared for commitment.
	 * @param preparingEnlistment A  object used to send a response to the transaction
	 * manager.
	 */
	function Prepare(preparingEnlistment:cs.system.transactions.PreparingEnlistment):Void;
	/**
	 * Notifies an enlisted object that a transaction is being rolled back (aborted).
	 * @param enlistment A  object used to send a response to the transaction manager.
	 */
	function Rollback(enlistment:cs.system.transactions.Enlistment):Void;
}
