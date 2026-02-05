package cs.system.diagnostics;

/** Instructs the debugger to step through the code instead of stepping into the code. This class cannot be inherited. */
@:native("System.Diagnostics.DebuggerStepThroughAttribute")
extern class DebuggerStepThroughAttribute extends cs.system.Attribute {
	function new():Void;
}
