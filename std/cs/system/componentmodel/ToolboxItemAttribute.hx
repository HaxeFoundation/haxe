package cs.system.componentmodel;

/** Represents an attribute of a toolbox item. */
@:native("System.ComponentModel.ToolboxItemAttribute")
extern class ToolboxItemAttribute extends cs.system.Attribute {
	/** Initializes a new instance of the  class and sets the type to the default, . This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.ToolboxItemAttribute;
	/** Initializes a new instance of the  class and sets the type to . This field is read-only. */
	static var None(default, never):cs.system.componentmodel.ToolboxItemAttribute;
	/**
	 * Gets or sets the type of the toolbox item.
	 * @return The type of the toolbox item.
	 */
	var ToolboxItemType(default, never):cs.system.Type;
	/**
	 * Gets or sets the name of the type of the current .
	 * @return The fully qualified type name of the current toolbox item.
	 */
	var ToolboxItemTypeName(default, never):String;
	@:overload(function(defaultType:Bool):Void {})
	@:overload(function(toolboxItemTypeName:String):Void {})
	function new(toolboxItemType:cs.system.Type):Void;
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
	 * Gets a value indicating whether the current value of the attribute is the
	 * default value for the attribute.
	 * @return if the current value of the attribute is the default; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
