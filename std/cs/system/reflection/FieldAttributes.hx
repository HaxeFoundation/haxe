package cs.system.reflection;

/** Specifies flags that describe the attributes of a field. */
@:native("System.Reflection.FieldAttributes")
extern enum abstract FieldAttributes(Int) {
	var Assembly = 3;
	var FamANDAssem = 2;
	var Family = 4;
	var FamORAssem = 5;
	var FieldAccessMask = 7;
	var HasDefault = 32768;
	var HasFieldMarshal = 4096;
	var HasFieldRVA = 256;
	var InitOnly = 32;
	var Literal = 64;
	var NotSerialized = 128;
	var PinvokeImpl = 8192;
	var Private = 1;
	var PrivateScope = 0;
	var Public = 6;
	var ReservedMask = 38144;
	var RTSpecialName = 1024;
	var SpecialName = 512;
	var Static = 16;
	@:op(A | B) static function or(lhs:FieldAttributes, rhs:FieldAttributes):FieldAttributes;
	@:op(A & B) static function and(lhs:FieldAttributes, rhs:FieldAttributes):FieldAttributes;
	@:op(A ^ B) static function xor(lhs:FieldAttributes, rhs:FieldAttributes):FieldAttributes;
	@:op(~A) static function complement(value:FieldAttributes):FieldAttributes;
}
