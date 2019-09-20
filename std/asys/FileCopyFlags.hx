package asys;

enum abstract FileCopyFlags(Int) {
	/**
		Fail if destination exists.
	**/
	var FailIfExists = 1 << 0;

	/**
		Copy-on-write reflink if possible.
	**/
	var COWClone = 1 << 1;

	/**
		Copy-on-write reflink or fail.
	**/
	var COWCloneForce = 1 << 2;

	inline function get_raw():Int return this;

	@:op(A | B)
	inline function join(other:FileCopyFlags) return this | other.get_raw();
}
