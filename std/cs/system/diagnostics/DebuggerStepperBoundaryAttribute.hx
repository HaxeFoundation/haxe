package cs.system.diagnostics;

/** Indicates the code following the attribute is to be executed in run, not step, mode. */
@:native("System.Diagnostics.DebuggerStepperBoundaryAttribute")
extern class DebuggerStepperBoundaryAttribute extends cs.system.Attribute {
	function new():Void;
}
