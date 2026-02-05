package cs.system.data;

/** Provides a description of the results of the query and its effect on the database. */
@:native("System.Data.CommandBehavior")
extern enum abstract CommandBehavior(Int) {
	var CloseConnection = 32;
	var Default = 0;
	var KeyInfo = 4;
	var SchemaOnly = 2;
	var SequentialAccess = 16;
	var SingleResult = 1;
	var SingleRow = 8;
	@:op(A | B) static function or(lhs:CommandBehavior, rhs:CommandBehavior):CommandBehavior;
	@:op(A & B) static function and(lhs:CommandBehavior, rhs:CommandBehavior):CommandBehavior;
	@:op(A ^ B) static function xor(lhs:CommandBehavior, rhs:CommandBehavior):CommandBehavior;
	@:op(~A) static function complement(value:CommandBehavior):CommandBehavior;
}
