package cs.system.runtime.compilerservices;

/** Represents a builder for asynchronous methods that return a . */
@:native("System.Runtime.CompilerServices.AsyncValueTaskMethodBuilder`1")
extern class AsyncValueTaskMethodBuilder_1<TResult> extends cs.system.ValueType {
	/**
	 * Gets the task for this builder.
	 * @return The task for this builder.
	 */
	var Task(default, never):cs.system.threading.tasks.ValueTask_1<TResult>;
	/**
	 * Creates an instance of the  struct.
	 * @return The initialized instance.
	 */
	static function Create<TResult>():cs.system.runtime.compilerservices.AsyncValueTaskMethodBuilder_1<TResult>;
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
	/**
	 * Marks the task as failed and binds the specified exception to the task.
	 * @param exception The exception to bind to the task.
	 */
	function SetException(exception:cs.system.Exception):Void;
	/** Marks the task as successfully completed. */
	function SetResult(result:TResult):Void;
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
