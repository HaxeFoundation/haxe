package cs.system.runtime.compilerservices;

/** Indicates whether a method in Visual Basic is marked with the  modifier. */
@:native("System.Runtime.CompilerServices.IteratorStateMachineAttribute")
extern class IteratorStateMachineAttribute extends cs.system.runtime.compilerservices.StateMachineAttribute {
	function new(stateMachineType:cs.system.Type):Void;
}
