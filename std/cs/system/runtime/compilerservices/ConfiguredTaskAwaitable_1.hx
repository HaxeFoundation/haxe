package cs.system.runtime.compilerservices;

/** Provides an awaitable object that enables configured awaits on a task. */
@:native("System.Runtime.CompilerServices.ConfiguredTaskAwaitable`1")
extern class ConfiguredTaskAwaitable_1<TResult> extends cs.system.ValueType {
	/**
	 * Returns an awaiter for this awaitable object.
	 * @return The awaiter.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.ConfiguredTaskAwaitable_1_ConfiguredTaskAwaiter<TResult>;
}
