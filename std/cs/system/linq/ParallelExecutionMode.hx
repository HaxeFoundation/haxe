package cs.system.linq;

/** The query execution mode is a hint that specifies how the system should handle performance trade-offs when parallelizing queries. */
@:native("System.Linq.ParallelExecutionMode")
extern enum ParallelExecutionMode {
	Default;
	ForceParallelism;
}
