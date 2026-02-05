package cs.system.reflection;

/** Marks each type of member that is defined as a derived class of . */
@:native("System.Reflection.MemberTypes")
extern enum abstract MemberTypes(Int) {
	var All = 191;
	var Constructor = 1;
	var Custom = 64;
	var Event = 2;
	var Field = 4;
	var Method = 8;
	var NestedType = 128;
	var Property = 16;
	var TypeInfo = 32;
	@:op(A | B) static function or(lhs:MemberTypes, rhs:MemberTypes):MemberTypes;
	@:op(A & B) static function and(lhs:MemberTypes, rhs:MemberTypes):MemberTypes;
	@:op(A ^ B) static function xor(lhs:MemberTypes, rhs:MemberTypes):MemberTypes;
	@:op(~A) static function complement(value:MemberTypes):MemberTypes;
}
