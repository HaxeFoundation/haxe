package cs.system.transactions;

/** Contains additional information that specifies transaction behaviors. */
@:native("System.Transactions.TransactionOptions")
extern class TransactionOptions extends cs.system.ValueType {
	/**
	 * Gets or sets the isolation level of the transaction.
	 * @return A  enumeration that specifies the isolation level of the transaction.
	 */
	var IsolationLevel(default, default):cs.system.transactions.IsolationLevel;
	/**
	 * Gets or sets the timeout period for the transaction.
	 * @return A  value that specifies the timeout period for the transaction.
	 */
	var Timeout(default, default):cs.system.TimeSpan;
	/**
	 * Tests whether two specified  instances are equivalent.
	 * @param x The  instance that is to the left of the equality operator.
	 * @param y The  instance that is to the right of the equality operator.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(x:cs.system.transactions.TransactionOptions, y:cs.system.transactions.TransactionOptions):Bool;
	/**
	 * Returns a value that indicates whether two  instances are not equal.
	 * @param x The  instance that is to the left of the equality operator.
	 * @param y The  instance that is to the right of the equality operator.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(x:cs.system.transactions.TransactionOptions, y:cs.system.transactions.TransactionOptions):Bool;
	/**
	 * Determines whether this  instance and the specified object are equal.
	 * @param obj The object to compare with this instance.
	 * @return if  and this  instance are identical; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
