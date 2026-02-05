package cs.system.runtime.serialization;

/** Defines a set of flags that specifies the source or destination context for the stream during serialization. */
@:native("System.Runtime.Serialization.StreamingContextStates")
extern enum abstract StreamingContextStates(Int) {
	var All = 255;
	var Clone = 64;
	var CrossAppDomain = 128;
	var CrossMachine = 2;
	var CrossProcess = 1;
	var File = 4;
	var Other = 32;
	var Persistence = 8;
	var Remoting = 16;
	@:op(A | B) static function or(lhs:StreamingContextStates, rhs:StreamingContextStates):StreamingContextStates;
	@:op(A & B) static function and(lhs:StreamingContextStates, rhs:StreamingContextStates):StreamingContextStates;
	@:op(A ^ B) static function xor(lhs:StreamingContextStates, rhs:StreamingContextStates):StreamingContextStates;
	@:op(~A) static function complement(value:StreamingContextStates):StreamingContextStates;
}
