package cs.system.componentmodel;

/** Specifies the filter string and filter type to use for a toolbox item. */
@:native("System.ComponentModel.ToolboxItemFilterAttribute")
extern class ToolboxItemFilterAttribute extends cs.system.Attribute {
	/**
	 * Gets the filter string for the toolbox item.
	 * @return The filter string for the toolbox item.
	 */
	var FilterString(default, never):String;
	/**
	 * Gets the type of the filter.
	 * @return A  that indicates the type of the filter.
	 */
	var FilterType(default, never):cs.system.componentmodel.ToolboxItemFilterType;
	@:overload(function(filterString:String):Void {})
	function new(filterString:String, filterType:cs.system.componentmodel.ToolboxItemFilterType):Void;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An  to compare with this instance or a null reference ( in Visual
	 * Basic).
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Indicates whether the specified object has a matching filter string.
	 * @param obj The object to test for a matching filter string.
	 * @return if the specified object has a matching filter string; otherwise, .
	 */
	function Match(obj:Dynamic):Bool;
	/**
	 * Returns a string that represents the current object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
