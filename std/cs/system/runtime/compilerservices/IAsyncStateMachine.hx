package cs.system.runtime.compilerservices;

/** Represents state machines that are generated for asynchronous methods. This type is intended for compiler use only. */
@:native("System.Runtime.CompilerServices.IAsyncStateMachine")
extern interface IAsyncStateMachine {
	/** Moves the state machine to its next state. */
	function MoveNext():Void;
	/**
	 * Configures the state machine with a heap-allocated replica.
	 * @param stateMachine The heap-allocated replica.
	 */
	function SetStateMachine(stateMachine:cs.system.runtime.compilerservices.IAsyncStateMachine):Void;
}
