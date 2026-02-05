package cs.system.runtime.constrainedexecution;

/** Instructs the native image generation service to prepare a method for inclusion in a constrained execution region (CER). */
@:native("System.Runtime.ConstrainedExecution.PrePrepareMethodAttribute")
extern class PrePrepareMethodAttribute extends cs.system.Attribute {
	function new():Void;
}
