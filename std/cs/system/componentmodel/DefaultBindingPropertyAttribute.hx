package cs.system.componentmodel;

/** Specifies the default binding property for a component. This class cannot be inherited. */
@:native("System.ComponentModel.DefaultBindingPropertyAttribute")
extern class DefaultBindingPropertyAttribute extends cs.system.Attribute {
	/** Represents the default value for the  class. */
	static var Default(default, never):cs.system.componentmodel.DefaultBindingPropertyAttribute;
	/**
	 * Gets the name of the default binding property for the component to which the  is
	 * bound.
	 * @return The name of the default binding property for the component to which the 
	 * is bound.
	 */
	var Name(default, never):String;
	@:overload(function():Void {})
	function new(name:String):Void;
	/**
	 * Determines whether the specified  is equal to the current  instance.
	 * @param obj The  to compare with the current  instance
	 * @return if the object is equal to the current instance; otherwise, , indicating
	 * they are not equal.
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
