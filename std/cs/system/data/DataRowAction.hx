package cs.system.data;

/** Describes an action performed on a . */
@:native("System.Data.DataRowAction")
extern enum abstract DataRowAction(Int) {
	var Add = 16;
	var Change = 2;
	var ChangeCurrentAndOriginal = 64;
	var ChangeOriginal = 32;
	var Commit = 8;
	var Delete = 1;
	var Nothing = 0;
	var Rollback = 4;
	@:op(A | B) static function or(lhs:DataRowAction, rhs:DataRowAction):DataRowAction;
	@:op(A & B) static function and(lhs:DataRowAction, rhs:DataRowAction):DataRowAction;
	@:op(A ^ B) static function xor(lhs:DataRowAction, rhs:DataRowAction):DataRowAction;
	@:op(~A) static function complement(value:DataRowAction):DataRowAction;
}
