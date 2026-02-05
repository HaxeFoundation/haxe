package cs.system.transactions;

/** Facilitates communication between an enlisted transaction participant and the transaction manager during the Prepare phase of the transaction. */
@:native("System.Transactions.PreparingEnlistment")
extern class PreparingEnlistment extends cs.system.transactions.Enlistment {
	@:overload(function():Void {})
	/** Indicates that the transaction should be rolled back. */
	function ForceRollback(e:cs.system.Exception):Void;
	/** Indicates that the transaction can be committed. */
	function Prepared():Void;
	/**
	 * Gets the recovery information of an enlistment.
	 * @return The recovery information of an enlistment.
	 */
	function RecoveryInformation():cs.NativeArray<cs.UInt8>;
}
