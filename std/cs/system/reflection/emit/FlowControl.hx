package cs.system.reflection.emit;

/** Describes how an instruction alters the flow of control. */
@:native("System.Reflection.Emit.FlowControl")
extern enum FlowControl {
	Branch;
	Break;
	Call;
	Cond_Branch;
	Meta;
	Next;
	Phi;
	Return;
	Throw;
}
