package cs.system;

/** Specifies the application elements on which it is valid to apply an attribute. */
@:native("System.AttributeTargets")
extern enum abstract AttributeTargets(Int) {
	var All = 32767;
	var Assembly = 1;
	var Class = 4;
	var Constructor = 32;
	var Delegate = 4096;
	var Enum = 16;
	var Event = 512;
	var Field = 256;
	var GenericParameter = 16384;
	var Interface = 1024;
	var Method = 64;
	var Module = 2;
	var Parameter = 2048;
	var Property = 128;
	var ReturnValue = 8192;
	var Struct = 8;
	@:op(A | B) static function or(lhs:AttributeTargets, rhs:AttributeTargets):AttributeTargets;
	@:op(A & B) static function and(lhs:AttributeTargets, rhs:AttributeTargets):AttributeTargets;
	@:op(A ^ B) static function xor(lhs:AttributeTargets, rhs:AttributeTargets):AttributeTargets;
	@:op(~A) static function complement(value:AttributeTargets):AttributeTargets;
}
