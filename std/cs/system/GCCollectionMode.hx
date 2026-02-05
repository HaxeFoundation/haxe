package cs.system;

/** Specifies the behavior for a forced garbage collection. */
@:native("System.GCCollectionMode")
extern enum abstract GCCollectionMode(Int) {
	var Default = 0;
	var Forced = 1;
	var Optimized = 2;
}
