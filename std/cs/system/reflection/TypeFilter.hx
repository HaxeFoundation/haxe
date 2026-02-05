package cs.system.reflection;

/**
 * Filters the classes represented in an array of  objects.
 * @param m The  object to which the filter is applied.
 * @param filterCriteria An arbitrary object used to filter the list.
 * @return to include the  in the filtered list; otherwise .
 */
@:native("System.Reflection.TypeFilter")
extern class TypeFilter extends cs.system.MulticastDelegate {
	function new(func:(m:cs.system.Type, filterCriteria:Dynamic)->Bool):Void;
	function Invoke(m:cs.system.Type, filterCriteria:Dynamic):Bool;
}
