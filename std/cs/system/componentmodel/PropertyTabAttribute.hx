package cs.system.componentmodel;

/** Identifies the property tab or tabs to display for the specified class or classes. */
@:native("System.ComponentModel.PropertyTabAttribute")
extern class PropertyTabAttribute extends cs.system.Attribute {
	/**
	 * Gets the types of tabs that this attribute uses.
	 * @return An array of types indicating the types of tabs that this attribute uses.
	 */
	var TabClasses(default, never):cs.NativeArray<cs.system.Type>;
	/**
	 * Gets the names of the tab classes that this attribute uses.
	 * @return The names of the tab classes that this attribute uses.
	 */
	var TabClassNames(default, never):cs.NativeArray<String>;
	/**
	 * Gets an array of tab scopes of each tab of this .
	 * @return An array of  objects that indicate the scopes of the tabs.
	 */
	var TabScopes(default, never):cs.NativeArray<cs.system.componentmodel.PropertyTabScope>;
	@:overload(function():Void {})
	@:overload(function(tabClassName:String):Void {})
	@:overload(function(tabClass:cs.system.Type):Void {})
	@:overload(function(tabClassName:String, tabScope:cs.system.componentmodel.PropertyTabScope):Void {})
	function new(tabClass:cs.system.Type, tabScope:cs.system.componentmodel.PropertyTabScope):Void;
	@:overload(function(other:cs.system.componentmodel.PropertyTabAttribute):Bool {})
	/**
	 * Returns a value indicating whether this instance is equal to a specified
	 * attribute.
	 * @param other A  to compare to this instance, or .
	 * @return if the  instances are equal; otherwise, .
	 */
	function Equals(other:Dynamic):Bool;
	/**
	 * Gets the hash code for this object.
	 * @return The hash code for the object the attribute belongs to.
	 */
	function GetHashCode():Int;
}
