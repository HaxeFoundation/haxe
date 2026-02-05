package cs.system.io.pipes;

/** Specifies the direction of the pipe. */
@:native("System.IO.Pipes.PipeDirection")
extern enum PipeDirection {
	In;
	InOut;
	Out;
}
