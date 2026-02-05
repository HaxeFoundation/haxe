package cs.system.reflection;

/**
 * Represents a delegate that is used to filter a list of members represented in an
 * array of  objects.
 * @param m The  object to which the filter is applied.
 * @param filterCriteria An arbitrary object used to filter the list.
 * @return to include the member in the filtered list; otherwise .
 */
@:native("System.Reflection.MemberFilter")
extern class MemberFilter extends cs.system.MulticastDelegate {
	function new(func:(m:cs.system.reflection.MemberInfo, filterCriteria:Dynamic)->Bool):Void;
	function Invoke(m:cs.system.reflection.MemberInfo, filterCriteria:Dynamic):Bool;
}
