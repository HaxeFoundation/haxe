package cs.system.runtime.interopservices;

/** Describes the original settings of the  in the COM type library from which the type was imported. */
@:native("System.Runtime.InteropServices.TypeLibTypeFlags")
extern enum abstract TypeLibTypeFlags(Int) {
	var FAggregatable = 1024;
	var FAppObject = 1;
	var FCanCreate = 2;
	var FControl = 32;
	var FDispatchable = 4096;
	var FDual = 64;
	var FHidden = 16;
	var FLicensed = 4;
	var FNonExtensible = 128;
	var FOleAutomation = 256;
	var FPreDeclId = 8;
	var FReplaceable = 2048;
	var FRestricted = 512;
	var FReverseBind = 8192;
	@:op(A | B) static function or(lhs:TypeLibTypeFlags, rhs:TypeLibTypeFlags):TypeLibTypeFlags;
	@:op(A & B) static function and(lhs:TypeLibTypeFlags, rhs:TypeLibTypeFlags):TypeLibTypeFlags;
	@:op(A ^ B) static function xor(lhs:TypeLibTypeFlags, rhs:TypeLibTypeFlags):TypeLibTypeFlags;
	@:op(~A) static function complement(value:TypeLibTypeFlags):TypeLibTypeFlags;
}
