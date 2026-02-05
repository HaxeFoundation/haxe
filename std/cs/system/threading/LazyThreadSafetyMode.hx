package cs.system.threading;

/** Specifies how a  instance synchronizes access among multiple threads. */
@:native("System.Threading.LazyThreadSafetyMode")
extern enum LazyThreadSafetyMode {
	ExecutionAndPublication;
	None;
	PublicationOnly;
}
