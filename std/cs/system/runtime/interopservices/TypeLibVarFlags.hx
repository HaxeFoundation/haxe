package cs.system.runtime.interopservices;

/** Describes the original settings of the  in the COM type library from which the variable was imported. */
@:native("System.Runtime.InteropServices.TypeLibVarFlags")
extern enum abstract TypeLibVarFlags(Int) {
	var FBindable = 4;
	var FDefaultBind = 32;
	var FDefaultCollelem = 256;
	var FDisplayBind = 16;
	var FHidden = 64;
	var FImmediateBind = 4096;
	var FNonBrowsable = 1024;
	var FReadOnly = 1;
	var FReplaceable = 2048;
	var FRequestEdit = 8;
	var FRestricted = 128;
	var FSource = 2;
	var FUiDefault = 512;
	@:op(A | B) static function or(lhs:TypeLibVarFlags, rhs:TypeLibVarFlags):TypeLibVarFlags;
	@:op(A & B) static function and(lhs:TypeLibVarFlags, rhs:TypeLibVarFlags):TypeLibVarFlags;
	@:op(A ^ B) static function xor(lhs:TypeLibVarFlags, rhs:TypeLibVarFlags):TypeLibVarFlags;
	@:op(~A) static function complement(value:TypeLibVarFlags):TypeLibVarFlags;
}
