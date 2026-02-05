package cs.system.componentmodel;

/** Specifies the name of the property that an implementer of  offers to other components. This class cannot be inherited */
@:native("System.ComponentModel.ProvidePropertyAttribute")
extern class ProvidePropertyAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of a property that this class provides.
	 * @return The name of a property that this class provides.
	 */
	var PropertyName(default, never):String;
	/**
	 * Gets the name of the data type this property can extend.
	 * @return The name of the data type this property can extend.
	 */
	var ReceiverTypeName(default, never):String;
	@:overload(function(propertyName:String, receiverTypeName:String):Void {})
	function new(propertyName:String, receiverType:cs.system.Type):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
