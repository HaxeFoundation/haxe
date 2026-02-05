package cs.system.runtime.compilerservices;

/** Indicates whether a method is an asynchronous iterator. */
@:native("System.Runtime.CompilerServices.AsyncIteratorStateMachineAttribute")
extern class AsyncIteratorStateMachineAttribute extends cs.system.runtime.compilerservices.StateMachineAttribute {
	function new(stateMachineType:cs.system.Type):Void;
}
