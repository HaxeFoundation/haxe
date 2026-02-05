package cs.system.reflection;

/** Specifies flags for method attributes. These flags are defined in the corhdr.h file. */
@:native("System.Reflection.MethodAttributes")
extern enum abstract MethodAttributes(Int) {
	var Abstract = 1024;
	var Assembly = 3;
	var CheckAccessOnOverride = 512;
	var FamANDAssem = 2;
	var Family = 4;
	var FamORAssem = 5;
	var Final = 32;
	var HasSecurity = 16384;
	var HideBySig = 128;
	var MemberAccessMask = 7;
	var NewSlot = 256;
	var PinvokeImpl = 8192;
	var Private = 1;
	var PrivateScope = 0;
	var Public = 6;
	var RequireSecObject = 32768;
	var ReservedMask = 53248;
	var ReuseSlot = 0;
	var RTSpecialName = 4096;
	var SpecialName = 2048;
	var Static = 16;
	var UnmanagedExport = 8;
	var Virtual = 64;
	var VtableLayoutMask = 256;
	@:op(A | B) static function or(lhs:MethodAttributes, rhs:MethodAttributes):MethodAttributes;
	@:op(A & B) static function and(lhs:MethodAttributes, rhs:MethodAttributes):MethodAttributes;
	@:op(A ^ B) static function xor(lhs:MethodAttributes, rhs:MethodAttributes):MethodAttributes;
	@:op(~A) static function complement(value:MethodAttributes):MethodAttributes;
}
