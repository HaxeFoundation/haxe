package cs.system.io.memorymappedfiles;

/** Specifies access rights to a memory-mapped file that is not associated with a file on disk. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedFileRights")
extern enum abstract MemoryMappedFileRights(Int) {
	var AccessSystemSecurity = 16777216;
	var ChangePermissions = 262144;
	var CopyOnWrite = 1;
	var Delete = 65536;
	var Execute = 8;
	var FullControl = 983055;
	var Read = 4;
	var ReadExecute = 12;
	var ReadPermissions = 131072;
	var ReadWrite = 6;
	var ReadWriteExecute = 14;
	var TakeOwnership = 524288;
	var Write = 2;
	@:op(A | B) static function or(lhs:MemoryMappedFileRights, rhs:MemoryMappedFileRights):MemoryMappedFileRights;
	@:op(A & B) static function and(lhs:MemoryMappedFileRights, rhs:MemoryMappedFileRights):MemoryMappedFileRights;
	@:op(A ^ B) static function xor(lhs:MemoryMappedFileRights, rhs:MemoryMappedFileRights):MemoryMappedFileRights;
	@:op(~A) static function complement(value:MemoryMappedFileRights):MemoryMappedFileRights;
}
