package cs.system.linq.expressions;

/** Describes the binding types that are used in  objects. */
@:native("System.Linq.Expressions.MemberBindingType")
extern enum abstract MemberBindingType(Int) {
	var Assignment = 0;
	var ListBinding = 2;
	var MemberBinding = 1;
}
