package cs.system.runtime.compilerservices;

/** Provides the context for waiting when asynchronously switching into a target environment. */
@:native("System.Runtime.CompilerServices.YieldAwaitable")
extern class YieldAwaitable extends cs.system.ValueType {
	/**
	 * Retrieves a  object  for this instance of the class.
	 * @return The object that is used to monitor the completion of an asynchronous
	 * operation.
	 */
	function GetAwaiter():cs.system.runtime.compilerservices.YieldAwaitable_YieldAwaiter;
}
