package cs.system.linq.expressions;

/** Provides the base class from which the classes that represent bindings that are used to initialize members of a newly created object derive. */
@:native("System.Linq.Expressions.MemberBinding")
extern class MemberBinding {
	/**
	 * Gets the type of binding that is represented.
	 * @return One of the  values.
	 */
	var BindingType(default, never):cs.system.linq.expressions.MemberBindingType;
	/**
	 * Gets the field or property to be initialized.
	 * @return The  that represents the field or property to be initialized.
	 */
	var Member(default, never):cs.system.reflection.MemberInfo;
	/**
	 * Returns a textual representation of the .
	 * @return A textual representation of the .
	 */
	function ToString():String;
}
