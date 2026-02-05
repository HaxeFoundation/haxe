package cs.system.runtime.interopservices;

/** Describes the original settings of the  in the COM type library from where this method was imported. */
@:native("System.Runtime.InteropServices.TypeLibFuncFlags")
extern enum abstract TypeLibFuncFlags(Int) {
	var FBindable = 4;
	var FDefaultBind = 32;
	var FDefaultCollelem = 256;
	var FDisplayBind = 16;
	var FHidden = 64;
	var FImmediateBind = 4096;
	var FNonBrowsable = 1024;
	var FReplaceable = 2048;
	var FRequestEdit = 8;
	var FRestricted = 1;
	var FSource = 2;
	var FUiDefault = 512;
	var FUsesGetLastError = 128;
	@:op(A | B) static function or(lhs:TypeLibFuncFlags, rhs:TypeLibFuncFlags):TypeLibFuncFlags;
	@:op(A & B) static function and(lhs:TypeLibFuncFlags, rhs:TypeLibFuncFlags):TypeLibFuncFlags;
	@:op(A ^ B) static function xor(lhs:TypeLibFuncFlags, rhs:TypeLibFuncFlags):TypeLibFuncFlags;
	@:op(~A) static function complement(value:TypeLibFuncFlags):TypeLibFuncFlags;
}
