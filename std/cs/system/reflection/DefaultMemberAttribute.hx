package cs.system.reflection;

/** Defines the member of a type that is the default member used by . */
@:native("System.Reflection.DefaultMemberAttribute")
extern class DefaultMemberAttribute extends cs.system.Attribute {
	/**
	 * Gets the name from the attribute.
	 * @return A string representing the member name.
	 */
	var MemberName(default, never):String;
	function new(memberName:String):Void;
}
