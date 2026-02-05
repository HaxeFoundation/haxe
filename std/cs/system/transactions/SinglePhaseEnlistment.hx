package cs.system.transactions;

/** Provides a set of callbacks that facilitate communication between a participant enlisted for Single Phase Commit and the transaction manager when the  notification is received. */
@:native("System.Transactions.SinglePhaseEnlistment")
extern class SinglePhaseEnlistment extends cs.system.transactions.Enlistment {
	@:overload(function():Void {})
	/** Represents a callback that is used to indicate to the transaction manager that the transaction should be rolled back. */
	function Aborted(e:cs.system.Exception):Void;
	/** Represents a callback that is used to indicate to the transaction manager that the SinglePhaseCommit was successful. */
	function Committed():Void;
	@:overload(function():Void {})
	/** Represents a callback that is used to indicate to the transaction manager that the status of the transaction is in doubt. */
	function InDoubt(e:cs.system.Exception):Void;
}
