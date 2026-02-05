package cs.system;

/** Specifies the behavior for a forced garbage collection. */
@:native("System.GCCollectionMode")
extern enum GCCollectionMode {
	Default;
	Forced;
	Optimized;
}
