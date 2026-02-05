package cs.system.reflection;

/** Describes the constraints on a generic type parameter of a generic type or method. */
@:native("System.Reflection.GenericParameterAttributes")
extern enum abstract GenericParameterAttributes(Int) {
	var Contravariant = 2;
	var Covariant = 1;
	var DefaultConstructorConstraint = 16;
	var None = 0;
	var NotNullableValueTypeConstraint = 8;
	var ReferenceTypeConstraint = 4;
	var SpecialConstraintMask = 28;
	var VarianceMask = 3;
	@:op(A | B) static function or(lhs:GenericParameterAttributes, rhs:GenericParameterAttributes):GenericParameterAttributes;
	@:op(A & B) static function and(lhs:GenericParameterAttributes, rhs:GenericParameterAttributes):GenericParameterAttributes;
	@:op(A ^ B) static function xor(lhs:GenericParameterAttributes, rhs:GenericParameterAttributes):GenericParameterAttributes;
	@:op(~A) static function complement(value:GenericParameterAttributes):GenericParameterAttributes;
}
