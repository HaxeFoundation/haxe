package cs.system.runtime.interopservices;

/** Specifies the paths that are used to search for DLLs that provide functions for platform invokes. */
@:native("System.Runtime.InteropServices.DllImportSearchPath")
extern enum abstract DllImportSearchPath(Int) {
	var ApplicationDirectory = 512;
	var AssemblyDirectory = 2;
	var LegacyBehavior = 0;
	var SafeDirectories = 4096;
	var System32 = 2048;
	var UseDllDirectoryForDependencies = 256;
	var UserDirectories = 1024;
	@:op(A | B) static function or(lhs:DllImportSearchPath, rhs:DllImportSearchPath):DllImportSearchPath;
	@:op(A & B) static function and(lhs:DllImportSearchPath, rhs:DllImportSearchPath):DllImportSearchPath;
	@:op(A ^ B) static function xor(lhs:DllImportSearchPath, rhs:DllImportSearchPath):DllImportSearchPath;
	@:op(~A) static function complement(value:DllImportSearchPath):DllImportSearchPath;
}
