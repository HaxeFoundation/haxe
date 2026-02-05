package cs.system.reflection;

/** Identifies kinds of exception-handling clauses. */
@:native("System.Reflection.ExceptionHandlingClauseOptions")
extern enum abstract ExceptionHandlingClauseOptions(Int) {
	var Clause = 0;
	var Fault = 4;
	var Filter = 1;
	var Finally = 2;
	@:op(A | B) static function or(lhs:ExceptionHandlingClauseOptions, rhs:ExceptionHandlingClauseOptions):ExceptionHandlingClauseOptions;
	@:op(A & B) static function and(lhs:ExceptionHandlingClauseOptions, rhs:ExceptionHandlingClauseOptions):ExceptionHandlingClauseOptions;
	@:op(A ^ B) static function xor(lhs:ExceptionHandlingClauseOptions, rhs:ExceptionHandlingClauseOptions):ExceptionHandlingClauseOptions;
	@:op(~A) static function complement(value:ExceptionHandlingClauseOptions):ExceptionHandlingClauseOptions;
}
