package cs.system.componentmodel;

/** Identifies a type as an object suitable for binding to an  object. This class cannot be inherited. */
@:native("System.ComponentModel.DataObjectAttribute")
extern class DataObjectAttribute extends cs.system.Attribute {
	/** Indicates that the class is suitable for binding to an  object at design time. This field is read-only. */
	static var DataObject(default, never):cs.system.componentmodel.DataObjectAttribute;
	/** Represents the default value of the  class, which indicates that the class is suitable for binding to an  object at design time. This field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DataObjectAttribute;
	/** Indicates that the class is not suitable for binding to an  object at design time. This field is read-only. */
	static var NonDataObject(default, never):cs.system.componentmodel.DataObjectAttribute;
	/**
	 * Gets a value indicating whether an object should be considered suitable for
	 * binding to an  object at design time.
	 * @return if the object should be considered suitable for binding to an  object;
	 * otherwise, .
	 */
	var IsDataObject(default, never):Bool;
	@:overload(function():Void {})
	function new(isDataObject:Bool):Void;
	/**
	 * Determines whether this instance of  fits the pattern of another object.
	 * @param obj An object to compare with this instance of .
	 * @return if this instance is the same as the instance specified by the 
	 * parameter; otherwise, .
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
