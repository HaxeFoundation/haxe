package cs.system.threading;

/** Specifies the execution states of a . */
@:native("System.Threading.ThreadState")
extern enum abstract ThreadState(Int) {
	var Aborted = 256;
	var AbortRequested = 128;
	var Background = 4;
	var Running = 0;
	var Stopped = 16;
	var StopRequested = 1;
	var Suspended = 64;
	var SuspendRequested = 2;
	var Unstarted = 8;
	var WaitSleepJoin = 32;
	@:op(A | B) static function or(lhs:ThreadState, rhs:ThreadState):ThreadState;
	@:op(A & B) static function and(lhs:ThreadState, rhs:ThreadState):ThreadState;
	@:op(A ^ B) static function xor(lhs:ThreadState, rhs:ThreadState):ThreadState;
	@:op(~A) static function complement(value:ThreadState):ThreadState;
}
