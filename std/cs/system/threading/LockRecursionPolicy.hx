package cs.system.threading;

/** Specifies whether a lock can be entered multiple times by the same thread. */
@:native("System.Threading.LockRecursionPolicy")
extern enum abstract LockRecursionPolicy(Int) {
	var NoRecursion = 0;
	var SupportsRecursion = 1;
}
