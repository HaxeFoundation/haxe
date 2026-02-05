package cs.system.reflection.emit;

/** Describes how an instruction alters the flow of control. */
@:native("System.Reflection.Emit.FlowControl")
extern enum abstract FlowControl(Int) {
	var Branch = 0;
	var Break = 1;
	var Call = 2;
	var Cond_Branch = 3;
	var Meta = 4;
	var Next = 5;
	var Phi = 6;
	var Return = 7;
	var Throw = 8;
}
