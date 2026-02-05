package cs.system.data;

/** Describes the current state of the connection to a data source. */
@:native("System.Data.ConnectionState")
extern enum abstract ConnectionState(Int) {
	var Broken = 16;
	var Closed = 0;
	var Connecting = 2;
	var Executing = 4;
	var Fetching = 8;
	var Open = 1;
	@:op(A | B) static function or(lhs:ConnectionState, rhs:ConnectionState):ConnectionState;
	@:op(A & B) static function and(lhs:ConnectionState, rhs:ConnectionState):ConnectionState;
	@:op(A ^ B) static function xor(lhs:ConnectionState, rhs:ConnectionState):ConnectionState;
	@:op(~A) static function complement(value:ConnectionState):ConnectionState;
}
