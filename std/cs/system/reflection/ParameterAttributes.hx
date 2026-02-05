package cs.system.reflection;

/** Defines the attributes that can be associated with a parameter. These are defined in CorHdr.h. */
@:native("System.Reflection.ParameterAttributes")
extern enum abstract ParameterAttributes(Int) {
	var HasDefault = 4096;
	var HasFieldMarshal = 8192;
	var In = 1;
	var Lcid = 4;
	var None = 0;
	var Optional = 16;
	var Out = 2;
	var Reserved3 = 16384;
	var Reserved4 = 32768;
	var ReservedMask = 61440;
	var Retval = 8;
	@:op(A | B) static function or(lhs:ParameterAttributes, rhs:ParameterAttributes):ParameterAttributes;
	@:op(A & B) static function and(lhs:ParameterAttributes, rhs:ParameterAttributes):ParameterAttributes;
	@:op(A ^ B) static function xor(lhs:ParameterAttributes, rhs:ParameterAttributes):ParameterAttributes;
	@:op(~A) static function complement(value:ParameterAttributes):ParameterAttributes;
}
