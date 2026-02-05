package cs.system.threading;

/** Indicates whether an  is reset automatically or manually after receiving a signal. */
@:native("System.Threading.EventResetMode")
extern enum EventResetMode {
	AutoReset;
	ManualReset;
}
