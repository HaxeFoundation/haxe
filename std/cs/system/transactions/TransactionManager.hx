package cs.system.transactions;

/** Contains methods used for transaction management. This class cannot be inherited. */
@:native("System.Transactions.TransactionManager")
extern class TransactionManager {
	/**
	 * Gets the default timeout interval for new transactions.
	 * @return A  value that specifies the timeout interval for new transactions.
	 */
	static var DefaultTimeout(default, never):cs.system.TimeSpan;
	/**
	 * Gets or sets a custom transaction factory.
	 * @return A  that contains a custom transaction factory.
	 */
	static var HostCurrentCallback(default, default):cs.system.transactions.HostCurrentTransactionCallback;
	/**
	 * Gets the default maximum timeout interval for new transactions.
	 * @return A  value that specifies the maximum timeout interval that is allowed
	 * when creating new transactions.
	 */
	static var MaximumTimeout(default, never):cs.system.TimeSpan;
	/**
	 * Notifies the transaction manager that a resource manager recovering from failure
	 * has finished reenlisting in all unresolved transactions.
	 * @param resourceManagerIdentifier A  that uniquely identifies the resource to be
	 * recovered from.
	 */
	static function RecoveryComplete(resourceManagerIdentifier:cs.system.Guid):Void;
	/**
	 * Reenlists a durable participant in a transaction.
	 * @param resourceManagerIdentifier A  that uniquely identifies the resource
	 * manager.
	 * @param recoveryInformation Contains additional information of recovery
	 * information.
	 * @param enlistmentNotification A resource object that implements  to receive
	 * notifications.
	 * @return An  that describes the enlistment.
	 */
	static function Reenlist(resourceManagerIdentifier:cs.system.Guid, recoveryInformation:cs.NativeArray<cs.UInt8>, enlistmentNotification:cs.system.transactions.IEnlistmentNotification):cs.system.transactions.Enlistment;
}
