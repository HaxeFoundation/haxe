package cs.system.io.pipes;

/** Specifies the direction of the pipe. */
@:native("System.IO.Pipes.PipeDirection")
extern enum abstract PipeDirection(Int) {
	var In = 1;
	var InOut = 3;
	var Out = 2;
}
