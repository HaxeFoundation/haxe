package cs.system.componentmodel;

/** Specifies the editor to use to change a property. This class cannot be inherited. */
@:native("System.ComponentModel.EditorAttribute")
extern class EditorAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the base class or interface serving as a lookup key for this
	 * editor.
	 * @return The name of the base class or interface serving as a lookup key for this
	 * editor.
	 */
	var EditorBaseTypeName(default, never):String;
	/**
	 * Gets the name of the editor class in the  format.
	 * @return The name of the editor class in the  format.
	 */
	var EditorTypeName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(typeName:String, baseTypeName:String):Void {})
	@:overload(function(typeName:String, baseType:cs.system.Type):Void {})
	function new(type:cs.system.Type, baseType:cs.system.Type):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current object;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
