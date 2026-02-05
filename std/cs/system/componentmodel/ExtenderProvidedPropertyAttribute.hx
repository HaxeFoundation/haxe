package cs.system.componentmodel;

/** Specifies a property that is offered by an extender provider. This class cannot be inherited. */
@:native("System.ComponentModel.ExtenderProvidedPropertyAttribute")
extern class ExtenderProvidedPropertyAttribute extends cs.system.Attribute {
	/**
	 * Gets the property that is being provided.
	 * @return A  encapsulating the property that is being provided.
	 */
	var ExtenderProperty(default, never):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Gets the extender provider that is providing the property.
	 * @return The  that is providing the property.
	 */
	var Provider(default, never):cs.system.componentmodel.IExtenderProvider;
	/**
	 * Gets the type of object that can receive the property.
	 * @return A  describing the type of object that can receive the property.
	 */
	var ReceiverType(default, never):cs.system.Type;
	function new():Void;
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
	 * Provides an indication whether the value of this instance is the default value
	 * for the derived class.
	 * @return if this instance is the default attribute for the class; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
