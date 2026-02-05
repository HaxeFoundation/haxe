package cs.system.transactions;

/** Represents a transaction. */
@:native("System.Transactions.Transaction")
extern class Transaction {
	/**
	 * Gets or sets the ambient transaction.
	 * @return A  that describes the current transaction.
	 */
	static var Current(default, default):cs.system.transactions.Transaction;
	/**
	 * Gets the isolation level of the transaction.
	 * @return One of the  values that indicates the isolation level of the
	 * transaction.
	 */
	var IsolationLevel(default, never):cs.system.transactions.IsolationLevel;
	/**
	 * Uniquely identifies the format of the byte[] returned by the Promote method when
	 * the transaction is promoted.
	 * @return A guid that uniquely identifies the format of the byte[] returned by the
	 * Promote method when the transaction is promoted.
	 */
	var PromoterType(default, never):cs.system.Guid;
	/**
	 * Retrieves additional information about a transaction.
	 * @return A  that contains additional information about the transaction.
	 */
	var TransactionInformation(default, never):cs.system.transactions.TransactionInformation;
	/**
	 * Tests whether two specified  instances are equivalent.
	 * @param x The  instance that is to the left of the equality operator.
	 * @param y The  instance that is to the right of the equality operator.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(x:cs.system.transactions.Transaction, y:cs.system.transactions.Transaction):Bool;
	/**
	 * Returns a value that indicates whether two  instances are not equal.
	 * @param x The  instance that is to the left of the inequality operator.
	 * @param y The  instance that is to the right of the inequality operator.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(x:cs.system.transactions.Transaction, y:cs.system.transactions.Transaction):Bool;
	/**
	 * Creates a clone of the transaction.
	 * @return A  that is a copy of the current transaction object.
	 */
	function Clone():cs.system.transactions.Transaction;
	/**
	 * Creates a dependent clone of the transaction.
	 * @param cloneOption A  that controls what kind of dependent transaction to
	 * create.
	 * @return A  that represents the dependent clone.
	 */
	function DependentClone(cloneOption:cs.system.transactions.DependentCloneOption):cs.system.transactions.DependentTransaction;
	/** Releases the resources that are held by the object. */
	function Dispose():Void;
	@:overload(function(resourceManagerIdentifier:cs.system.Guid, enlistmentNotification:cs.system.transactions.IEnlistmentNotification, enlistmentOptions:cs.system.transactions.EnlistmentOptions):cs.system.transactions.Enlistment {})
	/**
	 * Enlists a durable resource manager that supports two phase commit to participate
	 * in a transaction.
	 * @param resourceManagerIdentifier A unique identifier for a resource manager,
	 * which should persist across resource manager failure or reboot.
	 * @param enlistmentNotification An object that implements the  interface to
	 * receive two phase commit notifications.
	 * @param enlistmentOptions if the resource manager wants to perform additional
	 * work during the prepare phase.
	 * @return An  object that describes the enlistment.
	 */
	function EnlistDurable(resourceManagerIdentifier:cs.system.Guid, singlePhaseNotification:cs.system.transactions.ISinglePhaseNotification, enlistmentOptions:cs.system.transactions.EnlistmentOptions):cs.system.transactions.Enlistment;
	@:overload(function(promotableSinglePhaseNotification:cs.system.transactions.IPromotableSinglePhaseNotification):Bool {})
	/**
	 * Enlists a resource manager that has an internal transaction using a promotable
	 * single phase enlistment (PSPE).
	 * @param promotableSinglePhaseNotification A  interface implemented by the
	 * participant.
	 * @return A  interface implementation that describes the enlistment.
	 */
	function EnlistPromotableSinglePhase(promotableSinglePhaseNotification:cs.system.transactions.IPromotableSinglePhaseNotification, promoterType:cs.system.Guid):Bool;
	@:overload(function(enlistmentNotification:cs.system.transactions.IEnlistmentNotification, enlistmentOptions:cs.system.transactions.EnlistmentOptions):cs.system.transactions.Enlistment {})
	/**
	 * Enlists a volatile resource manager that supports two phase commit to
	 * participate in a transaction.
	 * @param enlistmentNotification An object that implements the  interface to
	 * receive two-phase commit notifications.
	 * @param enlistmentOptions if the resource manager wants to perform additional
	 * work during the prepare phase.
	 * @return An  object that describes the enlistment.
	 */
	function EnlistVolatile(singlePhaseNotification:cs.system.transactions.ISinglePhaseNotification, enlistmentOptions:cs.system.transactions.EnlistmentOptions):cs.system.transactions.Enlistment;
	/**
	 * Determines whether this transaction and the specified object are equal.
	 * @param obj The object to compare with this instance.
	 * @return if  and this transaction are identical; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets the  byte[] returned by the Promote method when the transaction is
	 * promoted.
	 * @return The  byte[] returned by the Promote method when the transaction is
	 * promoted.
	 */
	function GetPromotedToken():cs.NativeArray<cs.UInt8>;
	/**
	 * Promotes and enlists a durable resource manager that supports two phase commit
	 * to participate in a transaction.
	 * @param resourceManagerIdentifier A unique identifier for a resource manager,
	 * which should persist across resource manager failure or reboot.
	 * @param promotableNotification An object that acts as a commit delegate for a
	 * non-distributed transaction internal to a resource manager.
	 * @param enlistmentNotification An object that implements the  interface to
	 * receive two phase commit notifications.
	 * @param enlistmentOptions if the resource manager wants to perform additional
	 * work during the prepare phase.
	 */
	function PromoteAndEnlistDurable(resourceManagerIdentifier:cs.system.Guid, promotableNotification:cs.system.transactions.IPromotableSinglePhaseNotification, enlistmentNotification:cs.system.transactions.ISinglePhaseNotification, enlistmentOptions:cs.system.transactions.EnlistmentOptions):cs.system.transactions.Enlistment;
	@:overload(function():Void {})
	/** Rolls back (aborts) the transaction. */
	function Rollback(e:cs.system.Exception):Void;
	/**
	 * Sets the distributed transaction identifier generated by the non-MSDTC promoter.
	 * @param promotableNotification A  interface implemented by the participant.
	 * @param distributedTransactionIdentifier The identifier for the transaction used
	 * by the distributed transaction manager.
	 */
	function SetDistributedTransactionIdentifier(promotableNotification:cs.system.transactions.IPromotableSinglePhaseNotification, distributedTransactionIdentifier:cs.system.Guid):Void;
}
