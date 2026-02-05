package cs.system.data;

/** Describes the version of data in a . */
@:native("System.Data.DataViewRowState")
extern enum abstract DataViewRowState(Int) {
	var Added = 4;
	var CurrentRows = 22;
	var Deleted = 8;
	var ModifiedCurrent = 16;
	var ModifiedOriginal = 32;
	var None = 0;
	var OriginalRows = 42;
	var Unchanged = 2;
	@:op(A | B) static function or(lhs:DataViewRowState, rhs:DataViewRowState):DataViewRowState;
	@:op(A & B) static function and(lhs:DataViewRowState, rhs:DataViewRowState):DataViewRowState;
	@:op(A ^ B) static function xor(lhs:DataViewRowState, rhs:DataViewRowState):DataViewRowState;
	@:op(~A) static function complement(value:DataViewRowState):DataViewRowState;
}
