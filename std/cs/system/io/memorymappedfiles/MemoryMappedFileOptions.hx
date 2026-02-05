package cs.system.io.memorymappedfiles;

/** Provides memory allocation options for memory-mapped files. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedFileOptions")
extern enum abstract MemoryMappedFileOptions(Int) {
	var DelayAllocatePages = 67108864;
	var None = 0;
	@:op(A | B) static function or(lhs:MemoryMappedFileOptions, rhs:MemoryMappedFileOptions):MemoryMappedFileOptions;
	@:op(A & B) static function and(lhs:MemoryMappedFileOptions, rhs:MemoryMappedFileOptions):MemoryMappedFileOptions;
	@:op(A ^ B) static function xor(lhs:MemoryMappedFileOptions, rhs:MemoryMappedFileOptions):MemoryMappedFileOptions;
	@:op(~A) static function complement(value:MemoryMappedFileOptions):MemoryMappedFileOptions;
}
