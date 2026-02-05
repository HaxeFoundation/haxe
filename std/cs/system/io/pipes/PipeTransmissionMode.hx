package cs.system.io.pipes;

/** Specifies the transmission mode of the pipe. */
@:native("System.IO.Pipes.PipeTransmissionMode")
extern enum abstract PipeTransmissionMode(Int) {
	var Byte = 0;
	var Message = 1;
}
