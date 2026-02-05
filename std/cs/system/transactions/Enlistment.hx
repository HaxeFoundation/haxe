package cs.system.transactions;

/** Facilitates communication between an enlisted transaction participant and the transaction manager during the final phase of the transaction. */
@:native("System.Transactions.Enlistment")
extern class Enlistment {
	/** Indicates that the transaction participant has completed its work. */
	function Done():Void;
}
