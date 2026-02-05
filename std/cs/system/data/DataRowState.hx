package cs.system.data;

/** Gets the state of a  object. */
@:native("System.Data.DataRowState")
extern enum abstract DataRowState(Int) {
	var Added = 4;
	var Deleted = 8;
	var Detached = 1;
	var Modified = 16;
	var Unchanged = 2;
	@:op(A | B) static function or(lhs:DataRowState, rhs:DataRowState):DataRowState;
	@:op(A & B) static function and(lhs:DataRowState, rhs:DataRowState):DataRowState;
	@:op(A ^ B) static function xor(lhs:DataRowState, rhs:DataRowState):DataRowState;
	@:op(~A) static function complement(value:DataRowState):DataRowState;
}
