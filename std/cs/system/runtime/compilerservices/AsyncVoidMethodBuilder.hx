package cs.system.runtime.compilerservices;

/** Represents a builder for asynchronous methods that do not return a value. */
@:native("System.Runtime.CompilerServices.AsyncVoidMethodBuilder")
extern class AsyncVoidMethodBuilder extends cs.system.ValueType {
	/**
	 * Creates an instance of the  class.
	 * @return A new instance of the builder.
	 */
	static function Create():cs.system.runtime.compilerservices.AsyncVoidMethodBuilder;
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
	 * awaiter completes. This method can be called from partially trusted code.
	 * @param TAwaiter The type of the awaiter.
	 * @param TStateMachine The type of the state machine.
	 * @param awaiter The awaiter.
	 * @param stateMachine The state machine.
	 */
	function AwaitUnsafeOnCompleted<TAwaiter, TStateMachine>(awaiter:cs.Ref<TAwaiter>, stateMachine:cs.Ref<TStateMachine>):Void;
	/**
	 * Binds an exception to the method builder.
	 * @param exception The exception to bind.
	 */
	function SetException(exception:cs.system.Exception):Void;
	/** Marks the method builder as successfully completed. */
	function SetResult():Void;
	/**
	 * Associates the builder with the specified state machine.
	 * @param stateMachine The state machine instance to associate with the builder.
	 */
	function SetStateMachine(stateMachine:cs.system.runtime.compilerservices.IAsyncStateMachine):Void;
	/**
	 * Begins running the builder with the associated state machine.
	 * @param TStateMachine The type of the state machine.
	 * @param stateMachine The state machine instance, passed by reference.
	 */
	function Start<TStateMachine>(stateMachine:cs.Ref<TStateMachine>):Void;
}
