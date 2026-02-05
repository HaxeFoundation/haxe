package cs.system.transactions;

/** Determines whether the object should be enlisted during the prepare phase. */
@:native("System.Transactions.EnlistmentOptions")
extern enum abstract EnlistmentOptions(Int) {
	var EnlistDuringPrepareRequired = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:EnlistmentOptions, rhs:EnlistmentOptions):EnlistmentOptions;
	@:op(A & B) static function and(lhs:EnlistmentOptions, rhs:EnlistmentOptions):EnlistmentOptions;
	@:op(A ^ B) static function xor(lhs:EnlistmentOptions, rhs:EnlistmentOptions):EnlistmentOptions;
	@:op(~A) static function complement(value:EnlistmentOptions):EnlistmentOptions;
}
