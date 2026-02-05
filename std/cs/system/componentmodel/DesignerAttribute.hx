package cs.system.componentmodel;

/** Specifies the class used to implement design-time services for a component. */
@:native("System.ComponentModel.DesignerAttribute")
extern class DesignerAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the base type of this designer.
	 * @return The name of the base type of this designer.
	 */
	var DesignerBaseTypeName(default, never):String;
	/**
	 * Gets the name of the designer type associated with this designer attribute.
	 * @return The name of the designer type associated with this designer attribute.
	 */
	var DesignerTypeName(default, never):String;
	@:overload(function(designerTypeName:String):Void {})
	@:overload(function(designerType:cs.system.Type):Void {})
	@:overload(function(designerTypeName:String, designerBaseTypeName:String):Void {})
	@:overload(function(designerTypeName:String, designerBaseType:cs.system.Type):Void {})
	function new(designerType:cs.system.Type, designerBaseType:cs.system.Type):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
