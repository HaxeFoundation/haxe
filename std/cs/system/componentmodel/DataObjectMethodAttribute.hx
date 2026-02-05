package cs.system.componentmodel;

/** Identifies a data operation method exposed by a type, what type of operation the method performs, and whether the method is the default data method. This class cannot be inherited. */
@:native("System.ComponentModel.DataObjectMethodAttribute")
extern class DataObjectMethodAttribute extends cs.system.Attribute {
	/**
	 * Gets a value indicating whether the method that the  is applied to is the
	 * default data method exposed by the data object for a specific method type.
	 * @return if the method is the default method exposed by the object for a method
	 * type; otherwise, .
	 */
	var IsDefault(default, never):Bool;
	/**
	 * Gets a  value indicating the type of data operation the method performs.
	 * @return One of the  values that identifies the type of data operation performed
	 * by the method to which the  is applied.
	 */
	var MethodType(default, never):cs.system.componentmodel.DataObjectMethodType;
	@:overload(function(methodType:cs.system.componentmodel.DataObjectMethodType):Void {})
	function new(methodType:cs.system.componentmodel.DataObjectMethodType, isDefault:Bool):Void;
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
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
	 * Gets a value indicating whether this instance shares a common pattern with a
	 * specified attribute.
	 * @param obj An object to compare with this instance of .
	 * @return if this instance is the same as the instance specified by the 
	 * parameter; otherwise, .
	 */
	function Match(obj:Dynamic):Bool;
}
