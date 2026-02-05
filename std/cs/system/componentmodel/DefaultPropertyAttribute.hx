package cs.system.componentmodel;

/** Specifies the default property for a component. */
@:native("System.ComponentModel.DefaultPropertyAttribute")
extern class DefaultPropertyAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DefaultPropertyAttribute;
	/**
	 * Gets the name of the default property for the component this attribute is bound
	 * to.
	 * @return The name of the default property for the component this attribute is
	 * bound to. The default value is .
	 */
	var Name(default, never):String;
	function new(name:String):Void;
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
