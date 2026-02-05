package cs.system.io.memorymappedfiles;

/** Specifies access capabilities and restrictions for a memory-mapped file or view. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedFileAccess")
extern enum abstract MemoryMappedFileAccess(Int) {
	var CopyOnWrite = 3;
	var Read = 1;
	var ReadExecute = 4;
	var ReadWrite = 0;
	var ReadWriteExecute = 5;
	var Write = 2;
}
