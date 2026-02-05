package cs.system.io.pipes;

/** Specifies the transmission mode of the pipe. */
@:native("System.IO.Pipes.PipeTransmissionMode")
extern enum PipeTransmissionMode {
	Byte;
	Message;
}
