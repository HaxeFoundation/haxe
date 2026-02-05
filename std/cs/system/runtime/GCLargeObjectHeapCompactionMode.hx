package cs.system.runtime;

/** Indicates whether the next blocking garbage collection compacts the large object heap (LOH). */
@:native("System.Runtime.GCLargeObjectHeapCompactionMode")
extern enum abstract GCLargeObjectHeapCompactionMode(Int) {
	var CompactOnce = 2;
	var Default = 1;
}
