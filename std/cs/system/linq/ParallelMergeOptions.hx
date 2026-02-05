package cs.system.linq;

/** Specifies the preferred type of output merge to use in a query. In other words, it indicates how PLINQ should merge the results from the various partitions back into a single result sequence. This is a hint only, and may not be respected by the system when parallelizing all queries. */
@:native("System.Linq.ParallelMergeOptions")
extern enum ParallelMergeOptions {
	AutoBuffered;
	Default;
	FullyBuffered;
	NotBuffered;
}
