package cs.system.io.pipes;

/** Provides options for creating a  object. This enumeration has a  attribute that allows a bitwise combination of its member values. */
@:native("System.IO.Pipes.PipeOptions")
extern enum abstract PipeOptions(Int) {
	var Asynchronous = 1073741824;
	var CurrentUserOnly = 536870912;
	var None = 0;
	var WriteThrough = -2147483648;
	@:op(A | B) static function or(lhs:PipeOptions, rhs:PipeOptions):PipeOptions;
	@:op(A & B) static function and(lhs:PipeOptions, rhs:PipeOptions):PipeOptions;
	@:op(A ^ B) static function xor(lhs:PipeOptions, rhs:PipeOptions):PipeOptions;
	@:op(~A) static function complement(value:PipeOptions):PipeOptions;
}
