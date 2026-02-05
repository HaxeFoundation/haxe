package cs.system.runtime.compilerservices;

/** Provides a type that can be used to configure how awaits on an  are performed. */
@:native("System.Runtime.CompilerServices.ConfiguredAsyncDisposable")
extern class ConfiguredAsyncDisposable extends cs.system.ValueType {
	/**
	 * Asynchronously releases the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.runtime.compilerservices.ConfiguredValueTaskAwaitable;
}
