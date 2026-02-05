package cs.system.runtime.interopservices.comtypes;

/** Specifies how to invoke a function by . */
@:native("System.Runtime.InteropServices.ComTypes.INVOKEKIND")
extern enum abstract INVOKEKIND(Int) {
	var INVOKE_FUNC = 1;
	var INVOKE_PROPERTYGET = 2;
	var INVOKE_PROPERTYPUT = 4;
	var INVOKE_PROPERTYPUTREF = 8;
	@:op(A | B) static function or(lhs:INVOKEKIND, rhs:INVOKEKIND):INVOKEKIND;
	@:op(A & B) static function and(lhs:INVOKEKIND, rhs:INVOKEKIND):INVOKEKIND;
	@:op(A ^ B) static function xor(lhs:INVOKEKIND, rhs:INVOKEKIND):INVOKEKIND;
	@:op(~A) static function complement(value:INVOKEKIND):INVOKEKIND;
}
