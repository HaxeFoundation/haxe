package cs.system.reflection;

/** Defines the attributes that can be associated with a property. These attribute values are defined in corhdr.h. */
@:native("System.Reflection.PropertyAttributes")
extern enum abstract PropertyAttributes(Int) {
	var HasDefault = 4096;
	var None = 0;
	var Reserved2 = 8192;
	var Reserved3 = 16384;
	var Reserved4 = 32768;
	var ReservedMask = 62464;
	var RTSpecialName = 1024;
	var SpecialName = 512;
	@:op(A | B) static function or(lhs:PropertyAttributes, rhs:PropertyAttributes):PropertyAttributes;
	@:op(A & B) static function and(lhs:PropertyAttributes, rhs:PropertyAttributes):PropertyAttributes;
	@:op(A ^ B) static function xor(lhs:PropertyAttributes, rhs:PropertyAttributes):PropertyAttributes;
	@:op(~A) static function complement(value:PropertyAttributes):PropertyAttributes;
}
