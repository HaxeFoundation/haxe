package cs.system.io.memorymappedfiles;

/** Specifies access capabilities and restrictions for a memory-mapped file or view. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedFileAccess")
extern enum MemoryMappedFileAccess {
	CopyOnWrite;
	Read;
	ReadExecute;
	ReadWrite;
	ReadWriteExecute;
	Write;
}
