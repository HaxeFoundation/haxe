package cs.system.runtime.compilerservices;

/** Indicates whether a method is marked with either the Async or async modifier. */
@:native("System.Runtime.CompilerServices.AsyncStateMachineAttribute")
extern class AsyncStateMachineAttribute extends cs.system.runtime.compilerservices.StateMachineAttribute {
	function new(stateMachineType:cs.system.Type):Void;
}
