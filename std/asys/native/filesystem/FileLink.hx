package asys.native.filesystem;

enum abstract FileLink(Int) {
	/** Hard link. */
	var HardLink;
	/** Symbolic link. */
	var SymLink;
}