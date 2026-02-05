package cs.system.componentmodel;

/** Specifies what type to use as a converter for the object this attribute is bound to. */
@:native("System.ComponentModel.TypeConverterAttribute")
extern class TypeConverterAttribute extends cs.system.Attribute {
	/** Specifies the type to use as a converter for the object this attribute is bound to. */
	static var Default(default, never):cs.system.componentmodel.TypeConverterAttribute;
	/**
	 * Gets the fully qualified type name of the  to use as a converter for the object
	 * this attribute is bound to.
	 * @return The fully qualified type name of the  to use as a converter for the
	 * object this attribute is bound to, or an empty string ("") if none exists. The
	 * default value is an empty string ("").
	 */
	var ConverterTypeName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(typeName:String):Void {})
	function new(type:cs.system.Type):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current ;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
