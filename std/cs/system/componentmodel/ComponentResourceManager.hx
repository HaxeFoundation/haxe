package cs.system.componentmodel;

/** Provides simple functionality for enumerating resources for a component or object. The  class is a . */
@:native("System.ComponentModel.ComponentResourceManager")
extern class ComponentResourceManager extends cs.system.resources.ResourceManager {
	@:overload(function():Void {})
	function new(t:cs.system.Type):Void;
	@:overload(function(value:Dynamic, objectName:String):Void {})
	/**
	 * Applies a resource's value to the corresponding property of the object.
	 * @param value An  that contains the property value to be applied.
	 * @param objectName A  that contains the name of the object to look up in the
	 * resources.
	 */
	function ApplyResources(value:Dynamic, objectName:String, culture:cs.system.globalization.CultureInfo):Void;
}
