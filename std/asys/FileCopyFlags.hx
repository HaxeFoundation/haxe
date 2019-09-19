package asys;

enum abstract FileCopyFlags(Int) {
	var FailIfExists = 1 << 0; // fail if destination exists
	var COWClone = 1 << 1; // copy-on-write reflink if possible
	var COWCloneForce = 1 << 2; // copy-on-write reflink or fail

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:FileCopyFlags) return this | other.get_raw();
}
