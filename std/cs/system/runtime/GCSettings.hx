package cs.system.runtime;

/** Specifies the garbage collection settings for the current process. */
@:native("System.Runtime.GCSettings")
extern class GCSettings {
	/**
	 * Gets a value that indicates whether server garbage collection is enabled.
	 * @return if server garbage collection is enabled; otherwise, .
	 */
	static var IsServerGC(default, never):Bool;
	/**
	 * Gets or sets a value that indicates whether a full blocking garbage collection
	 * compacts the large object heap (LOH).
	 * @return One of the enumeration values that indicates whether a full blocking
	 * garbage collection compacts the LOH.
	 */
	static var LargeObjectHeapCompactionMode(default, default):cs.system.runtime.GCLargeObjectHeapCompactionMode;
	/**
	 * Gets or sets the current latency mode for garbage collection.
	 * @return One of the enumeration values that specifies the latency mode.
	 */
	static var LatencyMode(default, default):cs.system.runtime.GCLatencyMode;
}
