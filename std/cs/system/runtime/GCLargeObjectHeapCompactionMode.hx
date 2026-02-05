package cs.system.runtime;

/** Indicates whether the next blocking garbage collection compacts the large object heap (LOH). */
@:native("System.Runtime.GCLargeObjectHeapCompactionMode")
extern enum GCLargeObjectHeapCompactionMode {
	CompactOnce;
	Default;
}
