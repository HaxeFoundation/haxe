package cs.system.runtime.compilerservices;

/** Provides an awaitable object that enables configured awaits on a task. */
@:native("System.Runtime.CompilerServices.ConfiguredTaskAwaitable")
extern class ConfiguredTaskAwaitable extends cs.system.ValueType {
	/**
	 * Returns an awaiter for this awaitable object.
	 * @return The awaiter.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.ConfiguredTaskAwaitable_ConfiguredTaskAwaiter;
}
