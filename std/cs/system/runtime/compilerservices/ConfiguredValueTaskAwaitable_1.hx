package cs.system.runtime.compilerservices;

/** Provides an awaitable type that enables configured awaits on a . */
@:native("System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable`1")
extern class ConfiguredValueTaskAwaitable_1<TResult> extends cs.system.ValueType {
	/**
	 * Returns an awaiter for this  instance.
	 * @return The awaiter for this instance.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable_1_ConfiguredValueTaskAwaiter<TResult>;
}
