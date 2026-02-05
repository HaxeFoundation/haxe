package cs.system.threading;

/** Specifies how a  instance synchronizes access among multiple threads. */
@:native("System.Threading.LazyThreadSafetyMode")
extern enum abstract LazyThreadSafetyMode(Int) {
	var ExecutionAndPublication = 2;
	var None = 0;
	var PublicationOnly = 1;
}
