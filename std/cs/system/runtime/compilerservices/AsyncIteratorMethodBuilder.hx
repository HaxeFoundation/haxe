package cs.system.runtime.compilerservices;

/** Represents a builder for asynchronous iterators. */
@:native("System.Runtime.CompilerServices.AsyncIteratorMethodBuilder")
extern class AsyncIteratorMethodBuilder extends cs.system.ValueType {
	/**
	 * Creates an instance of the  struct.
	 * @return The initialized instance.
	 */
	static function Create():cs.system.runtime.compilerservices.AsyncIteratorMethodBuilder;
	/**
	 * Schedules the state machine to proceed to the next action when the specified
	 * awaiter completes.
	 * @param TAwaiter The type of the awaiter.
	 * @param TStateMachine The type of the state machine.
	 * @param awaiter The awaiter.
	 * @param stateMachine The state machine.
	 */
	function AwaitOnCompleted<TAwaiter, TStateMachine>(awaiter:cs.Ref<TAwaiter>, stateMachine:cs.Ref<TStateMachine>):Void;
	/**
	 * Schedules the state machine to proceed to the next action when the specified
	 * awaiter completes.
	 * @param TAwaiter The type of the awaiter.
	 * @param TStateMachine The type of the state machine.
	 * @param awaiter The awaiter.
	 * @param stateMachine The state machine.
	 */
	function AwaitUnsafeOnCompleted<TAwaiter, TStateMachine>(awaiter:cs.Ref<TAwaiter>, stateMachine:cs.Ref<TStateMachine>):Void;
	/** Marks iteration as being completed, whether successfully or otherwise. */
	function Complete():Void;
	/**
	 * Invokes  on the state machine while guarding the .
	 * @param TStateMachine The type of the state machine.
	 * @param stateMachine The state machine instance, passed by reference.
	 */
	function MoveNext<TStateMachine>(stateMachine:cs.Ref<TStateMachine>):Void;
}
